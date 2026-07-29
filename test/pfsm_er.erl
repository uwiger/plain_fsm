%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%==============================================================================
%% Test callback module using plain_fsm:extended_receive/1 (style C).
%% Also exercises hibernate/3, tail_apply/5, store_name/1, info/1,
%% format_status/2, terminate/2, and code_change/3 options.
%%==============================================================================
-module(pfsm_er).
-behaviour(plain_fsm).

-export([start/0, start_link/0, start_opt/1, start_opt/2]).
-export([call/2, cast/2]).
-export([data_vsn/0, code_change/3, format_status/2, terminate/2]).
%% Continuations / hibernate targets (must be exported)
-export([idle/1, after_hibernate/1, tail_cont/3]).

-include("plain_fsm.hrl").

-define(TIMEOUT, 5000).

data_vsn() ->
    1.

%% ---- start helpers ----------------------------------------------------------

start() ->
    plain_fsm:spawn(?MODULE, fun() -> boot(#{}) end).

start_link() ->
    plain_fsm:spawn_link(?MODULE, fun() -> boot(#{}) end).

%% start_opt modes: reply | noreply | bad
start_opt(Mode) ->
    start_opt(Mode, []).

start_opt(reply, Opts) ->
    plain_fsm:start_opt(
      ?MODULE,
      fun() ->
              {reply, {ok, self()}, fun() -> boot(#{mode => reply}) end}
      end,
      ?TIMEOUT,
      Opts);
start_opt(noreply, Opts) ->
    Parent = self(),
    plain_fsm:start_opt(
      ?MODULE,
      fun() ->
              %% Caller is expected to get no automatic ack; we ack ourselves
              %% after a short delay so tests can distinguish the modes.
              {noreply,
               fun() ->
                       proc_lib:init_ack(Parent, {ok, self()}),
                       boot(#{mode => noreply})
               end}
      end,
      ?TIMEOUT,
      Opts);
start_opt(no_ack, Opts) ->
    %% Never acks — parent should time out.
    plain_fsm:start_opt(
      ?MODULE,
      fun() ->
              {noreply, fun() -> boot(#{mode => no_ack}) end}
      end,
      200,
      Opts).

boot(S0) ->
    process_flag(trap_exit, true),
    idle(S0#{data => 0, events => []}).

%% ---- client API -------------------------------------------------------------

call(Pid, Req) ->
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, Req},
    receive
        {Ref, Reply} -> Reply
    after ?TIMEOUT ->
            error({timeout, call, Req})
    end.

cast(Pid, Msg) ->
    Pid ! {cast, Msg},
    ok.

%% ---- state: idle (handles system messages via extended_receive) ------------

idle(S) ->
    plain_fsm:extended_receive(
      receive
          {call, From, Req} ->
              idle(handle_call(Req, From, S));
          {cast, Msg} ->
              idle(handle_cast(Msg, S));
          %% used by suite to observe current_function expansion
          {call_mfa, From} ->
              reply(From, plain_fsm:current_function()),
              idle(S)
      end).

handle_call(ping, From, S) ->
    reply(From, pong),
    S;
handle_call(get, From, S) ->
    reply(From, S),
    S;
handle_call({set, Key, Val}, From, S) ->
    S1 = S#{Key => Val},
    reply(From, ok),
    S1;
handle_call({info, What}, From, S) ->
    reply(From, plain_fsm:info(What)),
    S;
handle_call({store_name, Name}, From, S) ->
    ok = plain_fsm:store_name(Name),
    reply(From, ok),
    S;
handle_call(hibernate, From, S) ->
    %% Remember who asked; reply after wake-up in after_hibernate/1
    plain_fsm:hibernate(?MODULE, after_hibernate, [S#{hib_from => From}]);
handle_call({tail_apply, Fun}, From, S) when is_function(Fun, 0) ->
    ?tail_apply(Fun, tail_cont, S#{tail_from => From});
handle_call(stop, From, _S) ->
    reply(From, ok),
    exit(normal);
handle_call(Other, From, S) ->
    reply(From, {error, {unknown, Other}}),
    S.

handle_cast({event, E}, S) ->
    Es = maps:get(events, S, []),
    S#{events => [E | Es]};
handle_cast(_, S) ->
    S.

after_hibernate(S) ->
    case maps:take(hib_from, S) of
        {From, S1} ->
            reply(From, {awake, plain_fsm:current_function()}),
            idle(S1);
        error ->
            idle(S)
    end.

%% ContF(Status, Result, State) for tail_apply
tail_cont(ok, Result, S) ->
    case maps:take(tail_from, S) of
        {From, S1} ->
            reply(From, {ok, Result}),
            idle(S1);
        error ->
            idle(S)
    end;
tail_cont(error, ThrowFun, S) when is_function(ThrowFun, 0) ->
    case maps:take(tail_from, S) of
        {From, S1} ->
            try ThrowFun() of
                _ ->
                    reply(From, {error, unexpected_return}),
                    idle(S1)
            catch
                Class:Reason ->
                    reply(From, {error, {Class, Reason}}),
                    idle(S1)
            end;
        error ->
            ThrowFun()
    end.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%% ---- behaviour callbacks ----------------------------------------------------

code_change(OldVsn, State, Extra) ->
    case Extra of
        {options, Opts} when is_list(Opts) ->
            {ok, State#{vsn => data_vsn(), old_vsn => OldVsn, extra => Extra},
             Opts};
        _ ->
            {ok, State#{vsn => data_vsn(), old_vsn => OldVsn, extra => Extra}}
    end.

format_status(_Opt, [_PDict, State]) ->
    [{data, [{"PFSM-ER State", State}]}].

terminate(Reason, State) ->
    %% Notify a watcher if the suite installed one in the process dict via
    %% a prior cast; for parent_EXIT tests we put a watcher pid in state.
    case maps:get(watcher, State, undefined) of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            Pid ! {terminated, self(), Reason, State},
            ok
    end.
