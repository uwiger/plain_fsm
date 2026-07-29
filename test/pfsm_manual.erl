%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%==============================================================================
%% Test callback modules styles (A) and (B):
%%   style_a/1 — explicit handle_system_msg/4 + parent_EXIT/2
%%   style_b/1 — catch-all handle_msg/3
%%==============================================================================
-module(pfsm_manual).
-behaviour(plain_fsm).

-export([start_link_a/0, start_link_b/0]).
-export([call/2]).
-export([data_vsn/0, code_change/3]).
-export([style_a/1, style_b/1]).

-define(TIMEOUT, 5000).

data_vsn() ->
    0.

start_link_a() ->
    plain_fsm:spawn_link(?MODULE, fun() ->
                                          process_flag(trap_exit, true),
                                          style_a(#{data => 0, style => a})
                                  end).

start_link_b() ->
    plain_fsm:spawn_link(?MODULE, fun() ->
                                          process_flag(trap_exit, true),
                                          style_b(#{data => 0, style => b})
                                  end).

call(Pid, Req) ->
    Ref = make_ref(),
    Pid ! {call, {self(), Ref}, Req},
    receive
        {Ref, Reply} -> Reply
    after ?TIMEOUT ->
            error({timeout, call, Req})
    end.

%% ---- (A) Explicit system-message handling -----------------------------------

style_a(S) ->
    Parent = plain_fsm:info(parent),
    receive
        {system, From, Req} ->
            plain_fsm:handle_system_msg(
              Req, From, S, fun(S1) -> style_a(S1) end);
        {'EXIT', Parent, Reason} ->
            plain_fsm:parent_EXIT(Reason, S);
        {call, From, ping} ->
            reply(From, {pong, a, maps:get(data, S)}),
            style_a(S);
        {call, From, get} ->
            reply(From, S),
            style_a(S);
        {call, From, {set, Data}} ->
            reply(From, ok),
            style_a(S#{data => Data});
        {call, From, stop} ->
            reply(From, ok),
            exit(normal)
    end.

%% ---- (B) Catch-all handle_msg/3 ---------------------------------------------

style_b(S) ->
    receive
        {call, From, ping} ->
            reply(From, {pong, b, maps:get(data, S)}),
            style_b(S);
        {call, From, get} ->
            reply(From, S),
            style_b(S);
        {call, From, {set, Data}} ->
            reply(From, ok),
            style_b(S#{data => Data});
        {call, From, stop} ->
            reply(From, ok),
            exit(normal);
        Msg ->
            plain_fsm:handle_msg(Msg, S, fun(S1) -> style_b(S1) end)
    end.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State#{upgraded => true}}.
