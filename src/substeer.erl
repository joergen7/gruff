%% -*- erlang -*-
%%
%% A basic worker pool factory for Erlang to demonstrate the expressive power of
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

-module(substeer).
-behavior( gen_pnet ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2,
          terminate/2, trigger/2] ).

-export( [place_lst/0, trsn_lst/0, init_marking/1, preset/1, is_enabled/2,
        fire/2] ).

-export( [start_link/0] ).

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  gen_pnet:start_link( ?MODULE, [] ).

%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _, _, _ ) -> {reply, {error, bad_msg}}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

terminate( _Reason, _NetState ) -> ok.

trigger( _, _ ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  [].

trsn_lst() ->
  [].

init_marking( _ ) ->
  [].

preset( _ ) ->
  [].

is_enabled( _, _ ) ->
  false.

fire( _, _ ) ->
  abort.


