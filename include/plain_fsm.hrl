%%==============================================================================
%% Copyright 2010 Erlang Solutions Ltd.
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
%% File    : plain_fsm.hrl
%% Author  : Ulf Wiger <ulf.wiger@ericsson.com>
%% Description : 
%%
%% Created : 29 Jan 2004 by Ulf Wiger <ulf.wiger@ericsson.com>
%%-------------------------------------------------------------------

-compile({parse_transform, plain_fsm_xform}).


-define(tail_apply(F,C,S),
	plain_fsm:tail_apply(F, ?MODULE:data_vsn(), ?MODULE, C, S)).
