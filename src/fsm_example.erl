%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%==============================================================================
%% Copyright 2014-23 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%-------------------------------------------------------------------
%% File    : fsm_example.erl
%% Author  : Ulf Wiger <ulf@wiger.net>
%% Description : Example illustrating the use of plain_fsm
%%
%% Created : 28 Jan 2004 by Ulf Wiger <ulf@wiger.net>
%%-------------------------------------------------------------------
-module(fsm_example).
-behaviour(plain_fsm).

%% Here's the plan:
%% plain_fsm.erl will include behaviour_info(),
%% the exported function plain_fsm:extended_receive/1
%% (which should never actually be called -- it will simply exit),
%% and the support functions spawn_link(), system_continue(),
%% system_code_change() et al.
%%
%% Users of plain_fsm must abide by a few rules:
%%
%% - at least one receive clause somewhere should be wrapped
%%   inside a plain_fsm:extended_receive(receive ... end).
%%   This will ensure that system messages are handled, including
%%   the shutdown protocol, without giving up selective receive.
%% - The function containing the extended_receive wrapper should
%%   have exactly one argument -- the 'State'.
%%
-export([spawn_link/0]).
-export([data_vsn/0, code_change/3]).
-include("plain_fsm.hrl").

%% needed because they are called via hibernate()
-export([a/1, b/1]).

-define(cur, plain_fsm:current_function()).
-define(dbg(F, A), io:fwrite("[~p]~p: " ++ F, [?LINE,?cur|A])).

data_vsn() ->
    5.

spawn_link() ->
    plain_fsm:spawn_link(?MODULE, fun() ->
                                          process_flag(trap_exit,true),
                                          idle(mystate)
                                  end).



idle(S) ->
    %% Pseudo-calls to plain_fsm:extended_receive can only occur in
    %% functions with arity 1 (the single argument being used as the state.)
    plain_fsm:extended_receive(
      receive
          a ->
              ?dbg("going to state a~n", []),
              plain_fsm:hibernate(?MODULE,a,[S]);
          b ->
              ?dbg("going to state b~n", []),
              b(S)
      after 10000 ->
              ?dbg("timeout in idle~n", []),
              idle(S)
      end).


a(S) ->
    receive
        b ->
            ?dbg("going from to state b~n", []),
            eventually_b(S);
        idle ->
            ?dbg("going to state idle~n", []),
            idle(S)
%       after 10000 ->
%             ?dbg("timeout in a~n", []),
%             idle(S)
    end.

b(S) ->
    receive
        a ->
            ?dbg("going to state a~n", []),
            a(S);
        idle ->
            ?dbg("going to state idle~n", []),
            idle(S)
    after 10000 ->
            ?dbg("timeout in b~n", []),
            idle(S)
    end.

code_change(_OldVsn, _State, _Extra) ->
    {ok, {newstate, data_vsn()}}.

%% Calls to hibernate can be anywhere in the code.
eventually_b(S) ->
    plain_fsm:hibernate(?MODULE,b,[S]).

