A behaviour/support library for writing plain Erlang FSMs.
==========================================================

Note on documentation
---------------------
This application now uses edown for github-friendly generation
of edoc markup. To rebuild documentation, use:
`./rebar get-deps`
`./rebar compile`
`./rebar doc`
 
This module implements an OTP behaviour for writing plain Erlang FSMs,
alleviating a long-standing gripe of mine that the OTP behaviours, for all
their power, force programmers into a coding style that is very much 
different from that taught in the Basic Erlang Course (or the book, or
online tutorials, ...) -- the type of programming that made us want to 
use Erlang in the first place.

The requirements that drove us away from plain Erlang programming
in the first place were:

- The need to support *system messages* to control upgrade,
   state inspection, shutdown, etc. The `plain_fsm` library solves this 
   in a reasonable way, I think.
- The need for debugging support. The debugging support in
   e.g. `gen_server` is, I believe, rendered obsolete by the new powerful
   trace support (and `dbg`) in later versions of OTP.
- In the case of gen_server, reducing the need to reinvent the 
   wheel, a valid point, but more so for e.g. the client side of 
   gen_server:call(). In a protocol state machine, the only thing that 
   really needs reusing is the handling of system messages.


However, the behaviours provided by OTP for FSM programming, 
`gen_server` and `gen_fsm` (`gen_server` is perhaps a more common 
choice than `gen_fsm`), both have the distinct drawback that you 
cannot normally start with a classic Erlang design and then migrate 
to a behaviour without significant code rewrite. In addition, the 
two behaviours are semantically different from the classic Erlang design

Using plain_fsm
---------------

First, write your state machine without worrying about OTP system
messages. Once you're happy with it, figure out where you really want 
to handle system messages. Normally, it will suffice to do it in a fairly
stable state. A good rule of thumb is that the top-level state machine
should handle system messages, while the transient (sub-) states 
shouldn't

In the states where you want to handle system messages, you have 
three choices:

### (A) Insert the system messages in the receive clause:#

    idle(S) ->
       Parent = plain_fsm:info(parent),
       receive
          {system, From, Req} ->
             plain_fsm:handle_system_msg(
                 From, Req, S, fun(S1) -> idle(S1) end);
          {'EXIT', Parent, Reason} ->
             plain_fsm:parent_EXIT(Reason, S);
          ... %% your original code here
       end.

This has the advantage that everyone can understand what's going on.
The part that plain_fsm.erl helps you with is the set of functions
`system_code_change()`, `system_continue()`, `code>system_shutdown()`,
`format_status()`, which are required callbacks when you handle system 
messages directly.

### (B) Handle system messages and unknown messages together: #

    idle(S) ->
       Parent = plain_fsm:info(parent),
       receive
          ... %% your original code here
          Msg ->
             plain_fsm:handle_msg(Msg, State, fun(S1) -> idle(S1) end)
       end.

This is quite convenient if the receive statement already has a 
'catch-all' clause, discarding unknown messages. 
`plain_fsm:handle_msg/3` will handle system messages properly
and ignore any other message.

### (C) Write a pseudo wrapper function around your receive clause:


    idle(S) ->
       plain_fsm:extended_receive(
          receive
            ... %% your original code
          end).

The function `plain_fsm:extended_receive/1` is replaced 
in a *parse_transform* into something that looks very much like
the previous program (A). The code, as it reads, requires the reader to
know that the transformation takes place, otherwise the semantics
would be confusing (you cannot solve the problem using a real function
that way.) On the plus side, this is a fairly small violation of both
the original code and Erlang's semantics.

*Note that for this to work, you must include "plain_fsm.hrl"
in your module.*

#### Example #

In the module fsm_example.erl (included in the plain_fsm package), 
we choose to handle system messages in the idle state. The example 
code is runnable, and supports suspend, resume, status inspection, 
and code change.

Imagine that the code initially looked like this:

    idle(S) ->
        receive
        a ->
            io:format("going to state a~n", []),
            a(S);
        b ->
            io:format("going to state b~n", []),
            b(S)
        after 10000 ->
            io:format("timeout in idle~n", []),
            idle(S)
        end).

The change required to handle system messages is as follows:

    idle(S) ->
        plain_fsm:extended_receive(
          receive
              a ->
                  io:format("going to state a~n", []),
                  a(S);
              b ->
                  io:format("going to state b~n", []),
                  b(S)
          after 10000 ->
                  io:format("timeout in idle~n", []),
                  idle(S)
          end).

In addition, we change the start function from, in this case:

    spawn_link() ->
        spawn_link(fun() ->
                           process_flag(trap_exit, true),
                           idle(mystate)
                   end).

Is changed into:

    spawn_link() ->
        plain_fsm:spawn_link(?MODULE, fun() ->
                                           process_flag(trap_exit,true),
                                           idle(mystate)
                                      end).

See also spawn/2 and spawn_opt/3 for information on other possible 
start functions.

To be fully compliant, you also need to supply a `code_change/3` function.
See behaviour_info/1 for details.