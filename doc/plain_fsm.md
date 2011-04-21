Module plain_fsm
================


<h1>Module plain_fsm</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


A behaviour/support library for writing plain Erlang FSMs.



__Authors:__ Ulf Wiger, ([`ulf.wiger@ericsson.com`](mailto:ulf.wiger@ericsson.com)).

<h2><a name="description">Description</a></h2>





This module implements an OTP behaviour for writing plain Erlang FSMs,
alleviating a long-standing gripe of mine that the OTP behaviours, for all
their power, force programmers into a coding style that is very much
different from that taught in the Basic Erlang Course (or the book, or
online tutorials, ...) -- the type of programming that made us want to
use Erlang in the first place.



Only in my old age have I begun to understand fully what a sacrifice
this is. See [
Programming Models for Concurrency ](pots/index.md) for a detailed discussion of
the issues involved.



The requirements that drove us away from plain Erlang programming
in the first place were:

* __The need to support _system messages___ to control upgrade,
state inspection, shutdown, etc. The plain_fsm library solves this in a
reasonable way, I think.

* __The need for debugging support__. The debugging support in
e.g. gen_server is, I believe, rendered obsolete by the new powerful
trace support (and dbg) in later versions of OTP.

* In the case of gen_server, __reducing the need to reinvent thewheel__, a valid point, but more so for e.g. the client side of
gen_server:call(). In a protocol state machine, the only thing that
really needs reusing is the handling of system messages.






However, the behaviours provided by OTP for FSM programming,
`gen_server` and `gen_fsm` (`gen_server`
is perhaps a more common choice than `gen_fsm`), both have the
distinct drawback that you cannot normally start with a classic
Erlang design and then migrate to a behaviour without significant
code rewrite. In addition, the two behaviours are semantically different
from the classic Erlang design



<h2>Using plain_fsm</h2>





First, write your state machine without worrying about OTP system
messages. Once you're happy with it, figure out where you really want
to handle system messages. Normally, it will suffice to do it in a fairly
stable state. A good rule of thumb is that the top-level state machine
should handle system messages, while the transient (sub-) states
shouldn't



In the states where you want to handle system messages, you have
three choices:



<h3>(A) Insert the system messages in the receive clause:</h3>



<pre>
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
  </pre>



This has the advantage that everyone can understand what's going on.
The part that plain_fsm.erl helps you with is the set of functions
`system_code_change()`, `system_continue()`,
`system_shutdown()`, `format_status()`, which
are required callbacks when you handle system messages directly.



<h3>(B) Handle system messages and unknown messages together:</h3>



<pre>
  idle(S) ->
     Parent = plain_fsm:info(parent),
     receive
        ... %% your original code here
        Msg ->
           plain_fsm:handle_msg(Msg, State, fun(S1) -> idle(S1) end)
     end.
  </pre>



This is quite convenient if the receive statement already has a
'catch-all' clause, discarding unknown messages.
`plain_fsm:handle_msg/3` will handle system messages properly
and ignore any other message.



<h3>(C) Write a pseudo wrapper function around your receive clause:</h3>



<pre>
  idle(S) ->
     plain_fsm:extended_receive(
        receive
           ... %% your original code
        end).
  </pre>



The function `plain_fsm:extended_receive/1` is replaced
in a _parse_transform_ into something that looks very much like
the previous program (A). The code, as it reads, requires the reader to
know that the transformation takes place, otherwise the semantics
would be confusing (you cannot solve the problem using a real function
that way.) On the plus side, this is a fairly small violation of both
the original code and Erlang's semantics.



_Note that for this to work, you must include "plain_fsm.hrl"in your module._



<h4>Example</h4>




In the module [fsm_example.erl](../src/fsm_example.erl)
(included in the plain_fsm package), we choose to handle system
messages in the idle state. The example code is runnable, and supports
suspend, resume, status inspection, and code change.


Imagine that the code initially looked like this:
<pre>
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
  </pre>



The change required to handle system messages is as follows:
<pre>
  idle(S) ->
      <a href="#extended_receive-1">plain_fsm:extended_receive</a>(
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
  </pre>



In addition, we change the start function from, in this case:
<pre>
  spawn_link() ->
      spawn_link(fun() ->
                         process_flag(trap_exit, true),
                         idle(mystate)
                 end).
  </pre>


Is changed into:
<pre>
  spawn_link() ->
      <a href="#spawn_link-2">plain_fsm:spawn_link</a>(?MODULE, fun() ->
                                            process_flag(trap_exit,true),
                                            idle(mystate)
                                    end).
  </pre>


See also [spawn/2](#spawn-2) and [spawn_opt/3](#spawn_opt-3)
for information on other possible start functions.


To be fully compliant, you also need to supply a code_change/3 function.
See [behaviour_info/1](#behaviour_info-1) for details.

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td>Defines which functions this behaviour expects to be exported from
the user's callback module.</td></tr><tr><td valign="top"><a href="#extended_receive-1">extended_receive/1</a></td><td>Virtual function used to wrap receive clauses.</td></tr><tr><td valign="top"><a href="#handle_msg-3">handle_msg/3</a></td><td>Called in a "catch-all" clause within a receive statement.</td></tr><tr><td valign="top"><a href="#handle_system_msg-4">handle_system_msg/4</a></td><td>Called when the process receives a system message.</td></tr><tr><td valign="top"><a href="#hibernate-3">hibernate/3</a></td><td>Virtual function used to wrap a call to the BIF erlang:hibernate/3.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>retrieves meta-data for the plain_fsm process.</td></tr><tr><td valign="top"><a href="#parent_EXIT-2">parent_EXIT/2</a></td><td>Handles parent termination properly.</td></tr><tr><td valign="top"><a href="#spawn-2">spawn/2</a></td><td>Equivalent to <code>proc_lib:spawn(StartF)</code>.</td></tr><tr><td valign="top"><a href="#spawn_link-2">spawn_link/2</a></td><td>Equivalent to <code>proc_lib:spawn_link(StartF)</code>.</td></tr><tr><td valign="top"><a href="#spawn_opt-3">spawn_opt/3</a></td><td>Equivalent to <code>proc_lib:spawn_opt(StartF, Opts)</code>.</td></tr><tr><td valign="top"><a href="#spawn_opt-4">spawn_opt/4</a></td><td>Equivalent to <code>proc_lib:spawn_opt(Node, StartF, Opts)</code>.</td></tr><tr><td valign="top"><a href="#start_opt-4">start_opt/4</a></td><td></td></tr><tr><td valign="top"><a href="#store_name-1">store_name/1</a></td><td>stores an internal name for the FSM
(for <code>sys:get_status()</code>).</td></tr><tr><td valign="top"><a href="#tail_apply-5">tail_apply/5</a></td><td>Helper function to dispatch blocking calls as tail calls.</td></tr><tr><td valign="top"><a href="#wake_up-5">wake_up/5</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="behaviour_info-1"></a>

<h3>behaviour_info/1</h3>





<pre>behaviour_info(Other::atom()) -> term()</pre>
<br></br>




Defines which functions this behaviour expects to be exported from
the user's callback module. plain_fsm requires only code_change/3 to
be present. The semantics of `Mod:code_change/3` are as follows:
<pre>
    code_change(OldVsn, State, Extra) -> {ok, NewState}.
  </pre>


The above code is just like it would look like in a gen_server callback
module.
<pre>
    code_change(OldVsn, State, Extra) -> {ok, NewState, Options}.
  </pre>


where `Options` may be any of 

* `{mod, module()}`, allowing you to switch callback
modules during a code change.

* `{name, name()}`, allowing you to rename the process
(note that you have to handle name registration yourself.)

* `{cont, atom() | function(1)}`, allowing you to provide
another continuation (point of entry into your own code after the
code change.)

<a name="extended_receive-1"></a>

<h3>extended_receive/1</h3>





<pre>extended_receive(Expr) -> VOID</pre>
<br></br>




Virtual function used to wrap receive clauses.


This function cannot be called directly, but is intended as a syntactic
wrapper around a receive clause. It will be transformed at compile time
to a set of receive patterns handling system messages and parent
termination according to the OTP rules. The transform requires that
the surrounding function has exactly one argument (the "State" or
"Loop Data".)


To trigger the parse_transform, include the file
`plain_fsm.hrl` (found in `plain_fsm/inc/`) in
your module, and the Erlang compiler must be able to find the module
`plain_fsm_xform.beam`. If `erlc` is used, this is
accomplished by adding `-pa .../plain_fsm/ebin` to the
`erlc` command.<a name="handle_msg-3"></a>

<h3>handle_msg/3</h3>





<pre>handle_msg(Other::Msg, State, Cont::<a href="#type-cont">cont()</a>) -> NEVER_RETURNS</pre>
<br></br>




Called in a "catch-all" clause within a receive statement.


This function never returns. It will handle system messages
properly and ignore anything else.
Example:
<pre>
  idle(S) ->
    receive
       ...
       Msg ->
           plain_fsm:handle_msg(Msg, S, fun(S1) ->
                                                 idle(S1)
                                        end)
    end.
  </pre>



Note that this function should _only_ be used if it is known
to be safe to discard unknown messages. In most state machines there should
be at least _one_ state where unknown messages are discarded; in
these states, the handle_msg/3 function can be a convenient way to
handle both unknown messages and system messages.



The `Cont` argument should be either a fun with one argument
(the new state), which jumps back into the user code in the proper place,
or it can be the name of a function (in this case, 'idle'). In the latter
case, the function in question must be exported; in the former case, this
is not necessary.<a name="handle_system_msg-4"></a>

<h3>handle_system_msg/4</h3>





<pre>handle_system_msg(Req, From, State, Cont::<a href="#type-cont">cont()</a>) -> NEVER_RETURNS</pre>
<br></br>




Called when the process receives a system message.


This function never returns. If the program handles system messages
explicitly, this function can be called to handle them in the plain_fsm
way. Example:
<pre>
  idle(S) ->
    receive
       {system, From, Req} ->
           plain_fsm:handle_system_msg(From, Req, S, fun(S1) ->
                                                            idle(S1)
                                                     end);
       ...
    end.
  </pre>


The `Cont` argument should be either a fun with one argument
(the new state), which jumps back into the user code in the proper place,
or it can be the name of a function (in this case, 'idle'). In the latter
case, the function in question must be exported; in the former case, this
is not necessary.<a name="hibernate-3"></a>

<h3>hibernate/3</h3>





<pre>hibernate(M::atom(), F::atom(), A::[IntState]) -> NEVER_RETURNS</pre>
<br></br>




Virtual function used to wrap a call to the BIF erlang:hibernate/3.


This function cannot be called directly, but translates to the call
`erlang:hibernate(plain_fsm,wake_up,[data_vsn(),Module,M,F,A])`
where `Module:data_vsn()` and `Module:code_change/3`
are expected to exist (the parse_transform will add and export the
function `data_vsn() -< 0`, if it doesn't already exist.)


The function `plain_fsm:wake_up/5` will begin by calling
`Module:data_vsn()`, and if it is the same as before, simply
call `apply(M,F,A)`. Otherwise, `Module:code_change(OldVsn,IntState, hibernate)` will be called first. This allows a plain_fsm
behaviour module to be "bootstrapped" to a new version during hibernation.
<a name="info-1"></a>

<h3>info/1</h3>





<pre>info(What::atom()) -> term()</pre>
<ul class="definitions"><li><pre>What = debug | name | mod | parent</pre></li></ul>



retrieves meta-data for the plain_fsm process.


Description of available meta-data:
<pre>
      debug : See the manual for sys.erl
      name  : Internal name, normally the same as the registered name.
              initially undefined, can be set via plain_fsm:store_name/1.
      mod   : Name of the callback module.
      parent: The pid() of the parent process.
    </pre><a name="parent_EXIT-2"></a>

<h3>parent_EXIT/2</h3>





<pre>parent_EXIT(Reason, State) -> EXIT</pre>
<br></br>




Handles parent termination properly.


This function is called when the parent of a plain_fsm instance dies.
The OTP rules state that the child should die with the same reason
as the parent (especially in the case of Reason='shutdown'.)<a name="spawn-2"></a>

<h3>spawn/2</h3>





<pre>spawn(Mod::atom(), StartF::function()) -> pid()</pre>
<br></br>




Equivalent to `proc_lib:spawn(StartF)`. This function also
initializes the plain_fsm meta-data.<a name="spawn_link-2"></a>

<h3>spawn_link/2</h3>





<pre>spawn_link(Mod::atom(), StartF::function()) -> pid()</pre>
<br></br>




Equivalent to `proc_lib:spawn_link(StartF)`.
This function also initializes the plain_fsm meta-data.<a name="spawn_opt-3"></a>

<h3>spawn_opt/3</h3>





<pre>spawn_opt(Mod::atom(), StartF::function(), Opts::list()) -> pid()</pre>
<br></br>




Equivalent to `proc_lib:spawn_opt(StartF, Opts)`.
This function also initializes the plain_fsm meta-data.<a name="spawn_opt-4"></a>

<h3>spawn_opt/4</h3>





<pre>spawn_opt(Node::atom(), Mod::atom(), StartF::function(), Opts::list()) -> pid()</pre>
<br></br>




Equivalent to `proc_lib:spawn_opt(Node, StartF, Opts)`.
This function also initializes the sysFsm meta-data.<a name="start_opt-4"></a>

<h3>start_opt/4</h3>





`start_opt(Mod, InitF, Timeout, Opts) -> any()`

<a name="store_name-1"></a>

<h3>store_name/1</h3>





<pre>store_name(Name::term()) -> ok</pre>
<br></br>




stores an internal name for the FSM
(for `sys:get_status()`).
This can be used if the FSM were started as an anonymous process
(the only kind currently supported).
Note that this function does not register the name. The name stored
is the one that shows up in sys:get_status/1. No restriction is made
here regarding the data type.<a name="tail_apply-5"></a>

<h3>tail_apply/5</h3>





<pre>tail_apply(F::Fun, OldVsn, Module, ContF, S) -> NEVER_RETURNS</pre>
<br></br>






Helper function to dispatch blocking calls as tail calls.
During code change, it can be a problem that processes lie in blocking
calls - say, e.g., to `gen_tcp:connect(...)`. If the module is reloaded,  
the calling function will still be on the call stack, and may eventually  
get the process killed (as the VM only holds two versions of the module).

This function is most easily called using the macro
`?tail_apply(F, ContF, S)`, which expands to
<pre>
  plain_fsm:tail_apply(F, ?MODULE:data_vsn(), ?MODULE, ContF, S)
  </pre>



In this case, `?MODULE:data_vsn()` will have been automatically
generated by plain_fsm, or is manually updated whenever the internal
representation of the state `S` is changed.



`ContF` represents an exported function in the calling module,
`ContF(Status, Result, S)`     
Status :: ok | error     
Result :: fun() | any()



If the call to `Fun()` fails, the exception (throw, error or exit) will
be caught, and `Result` will be a fun (arity 0), which can be called  
to "re-throw" the exception. This way, the continuation function can  
catch exceptions in its own try/catch pattern.



'Status' will be `error` if `Fun()` fails, otherwise `ok`.

Thus, the simplest implementation of `ContF` would be:
<pre>
  ContF(ok, Result, S) ->
      handle_result(Result, S);
  ContF(error, E, _S) ->
      E().
  </pre>

Note that this solution does not throw away the call stack, as
e.g. a call to `hibernate/3` does. Thus, it is basically only
tail-recursive as regards the calling function, placing
plain_fsm:tail_apply/5 on the call stack rather than a function
in the user module.<a name="wake_up-5"></a>

<h3>wake_up/5</h3>





`wake_up(OldVsn, Module, M, F, A) -> any()`

