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

-module( gruff ).
-behavior( gen_pnet ).

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/2] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/2,
          fire/3] ).

-export( [start_link/3] ).

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

-record( gruff_state, {nwrk, sup_pid} ).

%%====================================================================
%% API functions
%%====================================================================

start_link( WrkMod, WrkArgs, N ) ->
  gen_pnet:start_link( ?MODULE, {WrkMod, WrkArgs, N}, [] ).

%%====================================================================
%% Interface callback functions
%%====================================================================

code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

handle_call( _, _, _ ) -> {reply, {error, bad_msg}}.

handle_cast( _Request, _NetState ) -> noreply.

handle_info( _Request, _NetState ) -> noreply.

init( {WrkMod, WrkArgs, N} ) ->

  % trap exits
  process_flag( trap_exit, true ),

  % start supervisor process
  {ok, SupPid} = gruff_sup:start_link( WrkMod, WrkArgs ),

  % generate user info data structure
  GruffState = #gruff_state{ nwrk = N, sup_pid = SupPid },

  % generate gen_pnet initial data structure
  NetState = gen_pnet:new( ?MODULE, GruffState ),

  {ok, NetState}.

terminate( _Reason, _NetState ) -> ok.

trigger( _, _ ) -> pass.


%%====================================================================
%% Petri net callback functions
%%====================================================================

place_lst() ->
  ['Down', 'Checkout', 'Cancel', 'Checkin', 'Exit', 'Reply', 'Waiting', 'Busy',
   'Idle', 'Unstarted'].


trsn_lst() ->
  [down_busy, down_waiting, monitor, cancel_waiting, cancel_busy, free, alloc,
   exit_busy, exit_idle, start].


init_marking( 'Unstarted', #gruff_state{ nwrk = N } ) ->
  lists:duplicate( N, t );

init_marking( _, _ ) ->
  [].


preset( down_busy )      -> ['Down', 'Busy'];
preset( down_waiting )   -> ['Down', 'Waiting'];
preset( monitor )        -> ['Checkout'];
preset( cancel_waiting ) -> ['Cancel', 'Waiting'];
preset( cancel_busy )    -> ['Cancel', 'Busy'];
preset( free )           -> ['Checkin', 'Busy'];
preset( alloc )          -> ['Waiting', 'Idle'];
preset( exit_busy )      -> ['Exit', 'Busy'];
preset( exit_idle )      -> ['Exit', 'Idle'];
preset( start )          -> ['Unstarted'].


is_enabled( _, _ ) -> true.


fire( start, #{ 'Unstarted' := [t] }, #gruff_state{ sup_pid = SupPid } ) ->

  % start new worker under supervisor
  {ok, WrkPid} = supervisor:start_child( SupPid, [] ),

  % link to newly started worker process
  true = link( WrkPid ),

  {produce, #{ 'Idle' => [WrkPid] }}.


