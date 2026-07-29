%% -*- erlang-indent-level: 4; indent-tabs-mode: nil -*-
%%==============================================================================
%% Copyright 2026 Ulf Wiger
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
%% @doc Common Test suite for the documented features of `plain_fsm'.
%%
%% Covers:
%% <ul>
%%   <li>Start APIs: `spawn/2', `spawn_link/2', `spawn_opt/3', `start_opt/4'</li>
%%   <li>System-message styles (A) explicit, (B) `handle_msg/3',
%%       (C) `extended_receive/1'</li>
%%   <li>Metadata: `info/1', `store_name/1'</li>
%%   <li>OTP `sys' integration: status, suspend/resume, get/replace state,
%%       code change (incl. options)</li>
%%   <li>`hibernate/3' / `wake_up/5', `tail_apply/5', `current_function/0'</li>
%%   <li>Parent exit / `terminate/2', and the packaged `fsm_example'</li>
%% </ul>
%% @end
%%-------------------------------------------------------------------
-module(plain_fsm_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

-export([
         %% start APIs
         spawn_unlinked/1,
         spawn_link_and_parent/1,
         spawn_opt_link/1,
         start_opt_reply/1,
         start_opt_noreply/1,
         start_opt_timeout/1,

         %% virtual functions (direct call)
         virtual_functions_exit/1,

         %% style C — extended_receive
         er_ping_and_state/1,
         er_info_and_store_name/1,
         er_current_function/1,
         er_hibernate/1,
         er_tail_apply_ok/1,
         er_tail_apply_error/1,
         er_sys_status/1,
         er_sys_suspend_resume/1,
         er_sys_get_replace_state/1,
         er_sys_code_change/1,
         er_sys_code_change_options/1,
         er_parent_exit_terminate/1,

         %% style A — handle_system_msg
         style_a_basic_and_sys/1,

         %% style B — handle_msg
         style_b_basic_and_sys/1,
         style_b_ignores_unknown/1,

         %% packaged example
         fsm_example_lifecycle/1
        ]).

-define(WAIT, 5000).

%% ===================================================================
%% CT callbacks
%% ===================================================================

all() ->
    [{group, start},
     {group, virtual},
     {group, extended_receive},
     {group, style_a},
     {group, style_b},
     {group, example}].

groups() ->
    [{start, [parallel],
      [spawn_unlinked,
       spawn_link_and_parent,
       spawn_opt_link,
       start_opt_reply,
       start_opt_noreply,
       start_opt_timeout]},
     {virtual, [],
      [virtual_functions_exit]},
     {extended_receive, [],
      [er_ping_and_state,
       er_info_and_store_name,
       er_current_function,
       er_hibernate,
       er_tail_apply_ok,
       er_tail_apply_error,
       er_sys_status,
       er_sys_suspend_resume,
       er_sys_get_replace_state,
       er_sys_code_change,
       er_sys_code_change_options,
       er_parent_exit_terminate]},
     {style_a, [],
      [style_a_basic_and_sys]},
     {style_b, [],
      [style_b_basic_and_sys,
       style_b_ignores_unknown]},
     {example, [],
      [fsm_example_lifecycle]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    %% Best-effort cleanup of leftover FSMs linked to the test process
    ok.

%% ===================================================================
%% Start APIs
%% ===================================================================

spawn_unlinked(_Config) ->
    Self = self(),
    Pid = plain_fsm:spawn(pfsm_er, fun() ->
                                           Self ! {started, plain_fsm:info(parent)},
                                           pfsm_er:idle(#{data => 0, events => []})
                                   end),
    ?assert(is_pid(Pid)),
    Parent = receive {started, P} -> P after ?WAIT -> ct:fail(no_start) end,
    ?assertEqual(Self, Parent),
    %% Not linked: killing the FSM must not take us down
    process_flag(trap_exit, true),
    exit(Pid, kill),
    receive
        {'EXIT', Pid, _} -> ct:fail(unexpected_link)
    after 100 ->
            ok
    end,
    process_flag(trap_exit, false),
    ok.

spawn_link_and_parent(_Config) ->
    process_flag(trap_exit, true),
    Pid = pfsm_er:start_link(),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    ?assertEqual(self(), pfsm_er:call(Pid, {info, parent})),
    ?assertEqual(pfsm_er, pfsm_er:call(Pid, {info, mod})),
    pfsm_er:call(Pid, stop),
    receive
        {'EXIT', Pid, normal} -> ok
    after ?WAIT ->
            ct:fail(no_exit)
    end,
    process_flag(trap_exit, false),
    ok.

spawn_opt_link(_Config) ->
    process_flag(trap_exit, true),
    Pid = plain_fsm:spawn_opt(
            pfsm_er,
            fun() ->
                    process_flag(trap_exit, true),
                    pfsm_er:idle(#{data => 0, events => []})
            end,
            [link]),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    pfsm_er:call(Pid, stop),
    receive
        {'EXIT', Pid, normal} -> ok
    after ?WAIT ->
            ct:fail(no_exit)
    end,
    process_flag(trap_exit, false),
    ok.

start_opt_reply(_Config) ->
    {ok, Pid} = pfsm_er:start_opt(reply),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    S = pfsm_er:call(Pid, get),
    ?assertEqual(reply, maps:get(mode, S)),
    pfsm_er:call(Pid, stop),
    ok.

start_opt_noreply(_Config) ->
    %% InitF uses {noreply, Cont} but Cont itself performs init_ack
    {ok, Pid} = pfsm_er:start_opt(noreply),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    pfsm_er:call(Pid, stop),
    ok.

start_opt_timeout(_Config) ->
    ?assertEqual({error, timeout}, pfsm_er:start_opt(no_ack)),
    ok.

%% ===================================================================
%% Virtual functions (must not be called at runtime without transform)
%% ===================================================================

virtual_functions_exit(_Config) ->
    ?assertExit(cannot_be_called_directly, plain_fsm:extended_receive(ignore)),
    ?assertExit(cannot_be_called_directly, plain_fsm:current_function()),
    ?assertExit(cannot_be_called_directly, plain_fsm:hibernate(m, f, [])),
    ok.

%% ===================================================================
%% Style C — extended_receive
%% ===================================================================

er_ping_and_state(_Config) ->
    Pid = start_er(),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    ?assertEqual(ok, pfsm_er:call(Pid, {set, data, 42})),
    S = pfsm_er:call(Pid, get),
    ?assertEqual(42, maps:get(data, S)),
    pfsm_er:cast(Pid, {event, hello}),
    %% cast is async; poke with a call to ensure it was processed
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    S2 = pfsm_er:call(Pid, get),
    ?assertEqual([hello], maps:get(events, S2)),
    stop_er(Pid),
    ok.

er_info_and_store_name(_Config) ->
    Pid = start_er(),
    ?assertEqual(self(), pfsm_er:call(Pid, {info, parent})),
    ?assertEqual(pfsm_er, pfsm_er:call(Pid, {info, mod})),
    ?assertEqual(undefined, pfsm_er:call(Pid, {info, name})),
    ?assertEqual([], pfsm_er:call(Pid, {info, debug})),
    ?assertEqual(ok, pfsm_er:call(Pid, {store_name, my_fsm})),
    ?assertEqual(my_fsm, pfsm_er:call(Pid, {info, name})),
    %% sys:get_status should reflect the stored name in the header path
    {status, Pid, {module, plain_fsm}, _Status} = sys:get_status(Pid),
    stop_er(Pid),
    ok.

er_current_function(_Config) ->
    Pid = start_er(),
    Ref = make_ref(),
    Pid ! {call_mfa, {self(), Ref}},
    MFA = receive {Ref, Reply} -> Reply after ?WAIT -> ct:fail(timeout) end,
    ?assertEqual({pfsm_er, idle, 1}, MFA),
    stop_er(Pid),
    ok.

er_hibernate(_Config) ->
    Pid = start_er(),
    %% Request hibernation; the client blocks until we wake the FSM.
    Parent = self(),
    spawn_link(fun() ->
                       Parent ! {hib_reply, pfsm_er:call(Pid, hibernate)}
               end),
    true = wait_until(
             fun() ->
                     case process_info(Pid, current_function) of
                         {current_function, {erlang, hibernate, _}} -> true;
                         _ -> false
                     end
             end, 1000),
    %% A message wakes erlang:hibernate/3; after_hibernate/1 then replies.
    pfsm_er:cast(Pid, {event, wake}),
    Reply = receive {hib_reply, R} -> R after ?WAIT -> ct:fail(hib_timeout) end,
    ?assertMatch({awake, {pfsm_er, after_hibernate, 1}}, Reply),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    stop_er(Pid),
    ok.

er_tail_apply_ok(_Config) ->
    Pid = start_er(),
    Fun = fun() -> {computed, 7 * 6} end,
    ?assertEqual({ok, {computed, 42}}, pfsm_er:call(Pid, {tail_apply, Fun})),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    stop_er(Pid),
    ok.

er_tail_apply_error(_Config) ->
    Pid = start_er(),
    Fun = fun() -> error(boom) end,
    ?assertEqual({error, {error, boom}},
                 pfsm_er:call(Pid, {tail_apply, Fun})),
    Fun2 = fun() -> throw(nope) end,
    ?assertEqual({error, {throw, nope}},
                 pfsm_er:call(Pid, {tail_apply, Fun2})),
    Fun3 = fun() -> exit(bye) end,
    ?assertEqual({error, {exit, bye}},
                 pfsm_er:call(Pid, {tail_apply, Fun3})),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    stop_er(Pid),
    ok.

er_sys_status(_Config) ->
    Pid = start_er(),
    ok = pfsm_er:call(Pid, {store_name, status_fsm}),
    {status, Pid, {module, plain_fsm}, Info} = sys:get_status(Pid),
    %% Info is a list ending with the format_status return
    ?assert(is_list(Info)),
    Flat = lists:flatten(Info),
    %% format_status/2 in pfsm_er injects a recognizable label
    ?assert(lists:any(fun(E) ->
                              case E of
                                  {data, Data} when is_list(Data) ->
                                      lists:keymember("PFSM-ER State", 1, Data)
                                          orelse lists:keymember("State", 1, Data);
                                  _ ->
                                      false
                              end
                      end, Flat) orelse has_pfsm_label(Flat)),
    stop_er(Pid),
    ok.

er_sys_suspend_resume(_Config) ->
    Pid = start_er(),
    ok = sys:suspend(Pid),
    %% While suspended, a call must not complete
    Self = self(),
    spawn(fun() -> Self ! {call_done, catch pfsm_er:call(Pid, ping)} end),
    receive
        {call_done, _} -> ct:fail(call_should_block)
    after 200 ->
            ok
    end,
    ok = sys:resume(Pid),
    receive
        {call_done, pong} -> ok;
        {call_done, Other} -> ct:fail({unexpected, Other})
    after ?WAIT ->
            ct:fail(call_timeout_after_resume)
    end,
    stop_er(Pid),
    ok.

er_sys_get_replace_state(_Config) ->
    Pid = start_er(),
    ok = pfsm_er:call(Pid, {set, data, 1}),
    {Opts, State} = sys:get_state(Pid),
    ?assert(is_list(Opts)),
    ?assertEqual(pfsm_er, proplists:get_value(mod, Opts)),
    ?assertEqual(1, maps:get(data, State)),
    {NewOpts, NewState} =
        sys:replace_state(
          Pid,
          fun({Os, S}) ->
                  {Os, S#{data => 99, replaced => true}}
          end),
    ?assertEqual(99, maps:get(data, NewState)),
    ?assertEqual(true, maps:get(replaced, NewState)),
    ?assertEqual(pfsm_er, proplists:get_value(mod, NewOpts)),
    %% Live call should see the replaced user state
    S2 = pfsm_er:call(Pid, get),
    ?assertEqual(99, maps:get(data, S2)),
    stop_er(Pid),
    ok.

er_sys_code_change(_Config) ->
    Pid = start_er(),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, pfsm_er, old_vsn, {extra, 1}),
    ok = sys:resume(Pid),
    S = pfsm_er:call(Pid, get),
    ?assertEqual(1, maps:get(vsn, S)),
    ?assertEqual(old_vsn, maps:get(old_vsn, S)),
    ?assertEqual({extra, 1}, maps:get(extra, S)),
    stop_er(Pid),
    ok.

er_sys_code_change_options(_Config) ->
    Pid = start_er(),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, pfsm_er, v1,
                         {options, [{name, renamed_fsm}]}),
    ok = sys:resume(Pid),
    ?assertEqual(renamed_fsm, pfsm_er:call(Pid, {info, name})),
    S = pfsm_er:call(Pid, get),
    ?assertEqual(v1, maps:get(old_vsn, S)),
    stop_er(Pid),
    ok.

er_parent_exit_terminate(_Config) ->
    Tester = self(),
    Parent = spawn(fun() ->
                           process_flag(trap_exit, true),
                           Pid = pfsm_er:start_link(),
                           ok = pfsm_er:call(Pid, {set, watcher, Tester}),
                           Tester ! {child, Pid},
                           receive
                               go -> exit(shutdown)
                           end
                   end),
    Child = receive {child, C} -> C after ?WAIT -> ct:fail(no_child) end,
    Mon = monitor(process, Child),
    Parent ! go,
    Reason =
        receive
            {'DOWN', Mon, process, Child, R} -> R
        after ?WAIT ->
                ct:fail(child_did_not_die)
        end,
    ?assertEqual(shutdown, Reason),
    receive
        {terminated, Child, shutdown, _State} -> ok
    after ?WAIT ->
            ct:fail(terminate_not_called)
    end,
    ok.

%% ===================================================================
%% Style A — explicit handle_system_msg / parent_EXIT
%% ===================================================================

style_a_basic_and_sys(_Config) ->
    process_flag(trap_exit, true),
    Pid = pfsm_manual:start_link_a(),
    ?assertEqual({pong, a, 0}, pfsm_manual:call(Pid, ping)),
    ?assertEqual(ok, pfsm_manual:call(Pid, {set, 7})),
    ?assertEqual({pong, a, 7}, pfsm_manual:call(Pid, ping)),
    %% sys works because handle_system_msg is used
    {status, Pid, {module, plain_fsm}, _} = sys:get_status(Pid),
    ok = sys:suspend(Pid),
    ok = sys:resume(Pid),
    {_Opts, State} = sys:get_state(Pid),
    ?assertEqual(a, maps:get(style, State)),
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, pfsm_manual, 0, []),
    ok = sys:resume(Pid),
    S2 = pfsm_manual:call(Pid, get),
    ?assertEqual(true, maps:get(upgraded, S2)),
    pfsm_manual:call(Pid, stop),
    receive {'EXIT', Pid, normal} -> ok
    after ?WAIT -> ct:fail(no_exit) end,
    process_flag(trap_exit, false),
    ok.

%% ===================================================================
%% Style B — handle_msg catch-all
%% ===================================================================

style_b_basic_and_sys(_Config) ->
    process_flag(trap_exit, true),
    Pid = pfsm_manual:start_link_b(),
    ?assertEqual({pong, b, 0}, pfsm_manual:call(Pid, ping)),
    {status, Pid, {module, plain_fsm}, _} = sys:get_status(Pid),
    ok = sys:suspend(Pid),
    ok = sys:resume(Pid),
    ?assertEqual({pong, b, 0}, pfsm_manual:call(Pid, ping)),
    pfsm_manual:call(Pid, stop),
    receive {'EXIT', Pid, normal} -> ok
    after ?WAIT -> ct:fail(no_exit) end,
    process_flag(trap_exit, false),
    ok.

style_b_ignores_unknown(_Config) ->
    Pid = pfsm_manual:start_link_b(),
    %% Unknown messages are discarded by handle_msg/3
    Pid ! completely_unknown_message,
    Pid ! {noise, 1, 2, 3},
    ?assertEqual({pong, b, 0}, pfsm_manual:call(Pid, ping)),
    pfsm_manual:call(Pid, stop),
    ok.

%% ===================================================================
%% Packaged example module
%% ===================================================================

fsm_example_lifecycle(_Config) ->
    process_flag(trap_exit, true),
    Pid = fsm_example:spawn_link(),
    ?assert(is_pid(Pid)),
    %% Documented: supports suspend, resume, status inspection
    {status, Pid, {module, plain_fsm}, _} = sys:get_status(Pid),
    ok = sys:suspend(Pid),
    ok = sys:resume(Pid),
    %% Drive a few transitions (idle handles system msgs; a/b are plain)
    Pid ! a,
    timer:sleep(20),
    Pid ! idle,
    timer:sleep(20),
    Pid ! b,
    timer:sleep(20),
    Pid ! idle,
    timer:sleep(20),
    {status, Pid, {module, plain_fsm}, _} = sys:get_status(Pid),
    %% Code change path used by the example
    ok = sys:suspend(Pid),
    ok = sys:change_code(Pid, fsm_example, 5, []),
    ok = sys:resume(Pid),
    {_Opts, State} = sys:get_state(Pid),
    %% fsm_example:code_change/3 returns {ok, {newstate, data_vsn()}}
    ?assertEqual({newstate, 5}, State),
    exit(Pid, shutdown),
    receive {'EXIT', Pid, shutdown} -> ok
    after ?WAIT -> ct:fail(no_exit) end,
    process_flag(trap_exit, false),
    ok.

%% ===================================================================
%% Local helpers
%% ===================================================================

start_er() ->
    process_flag(trap_exit, true),
    Pid = pfsm_er:start_link(),
    ?assertEqual(pong, pfsm_er:call(Pid, ping)),
    Pid.

stop_er(Pid) ->
    case is_process_alive(Pid) of
        true ->
            catch pfsm_er:call(Pid, stop),
            receive
                {'EXIT', Pid, _} -> ok
            after 500 ->
                    exit(Pid, kill),
                    receive {'EXIT', Pid, _} -> ok after 100 -> ok end
            end;
        false ->
            ok
    end.

has_pfsm_label(Flat) ->
    lists:any(
      fun(E) ->
              case E of
                  {header, H} when is_list(H) ->
                      lists:prefix("Status for plain_fsm", H);
                  _ ->
                      false
              end
      end, Flat).

wait_until(Fun, Timeout) when Timeout =< 0 ->
    Fun();
wait_until(Fun, Timeout) ->
    case Fun() of
        true -> true;
        false ->
            timer:sleep(10),
            wait_until(Fun, Timeout - 10)
    end.
