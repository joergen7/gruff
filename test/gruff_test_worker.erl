%% -*- erlang -*-
%%
%% A basic wrk pool factory for Erlang to demonstrate the expressive power of
%% gen_pnet.
%%
%% Copyright 2017 Jorgen Brandt
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author Jorgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2017 Jorgen Brandt
%%
%% @end
%% -------------------------------------------------------------------

-module( gruff_test_worker ).
-behaviour( gen_server ).
-behaviour( gruff_wrk ).

-export( [start_link/1] ).
-export( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3] ).

start_link( _ ) -> gen_server:start_link( ?MODULE, [], [] ).

init( [] ) -> {ok, undefined}.

handle_call( die, _From, State )    -> {stop, {error, died}, dead, State};
handle_call( _Event, _From, State ) -> {reply, ok, State}.

handle_cast( _Event, State ) -> {noreply, State}.

handle_info( _Info, State ) -> {noreply, State}.

terminate( _Reason, _State ) -> ok.

code_change( _OldVsn, State, _Extra ) -> {ok, State}.