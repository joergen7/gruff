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

%%====================================================================
%% Exports
%%====================================================================

-export( [code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1,
          terminate/2, trigger/3] ).

-export( [place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/2,
          fire/3] ).

-export( [start_link/3] ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% Record definitions
%%====================================================================

-record( gruff_state, {nwrk, sup_pid} ).

%%====================================================================
%% Macro definitions
%%====================================================================

-define( TIMEOUT, 5000 ).

%%====================================================================
%% API functions
%%====================================================================

start_link( WrkMod, WrkArgs, N )
when is_atom( WrkMod ), is_integer( N ), N > 0 ->
  gen_pnet:start_link( ?MODULE, {WrkMod, WrkArgs, N}, [] ).

checkout( Pool ) ->
  checkout( Pool, ?TIMEOUT ).

checkout( Pool, Timeout ) ->
  R = make_ref(),
  try
    gen_pnet:call( Pool, {checkout, R}, Timeout )
  catch
    _:Reason ->
      ok = gen_pnet:cast( Pool, {cancel, R} ),
      {error, Reason}
  end.

checkin( Pool, WrkPid ) when is_pid( WrkPid ) ->
  gen_pnet:cast( Pool, {checkin, WrkPid} ).

%%====================================================================
%% Actor interface callback functions
%%====================================================================

%% @private
code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.

%% @private
handle_call( {checkout, R}, From, NetState ) when is_reference( R ) ->
  {noreply, #{}, #{ 'Checkout' => [{From, R}] }};

handle_call( _Request, _From, _NetState ) ->
  {reply, {error, bad_msg}}.


%% @private
handle_cast( {cancel, R}, NetState ) when is_reference( R ) ->
  {noreply, #{}, #{ 'Cancel' => [R]}};

handle_cast( {checkin, P}, NetState ) when is_pid( P ) ->
  {noreply, #{}, #{ 'Checkin' => [P] }};

handle_cast( _Request, _NetState ) -> noreply.

%% @private
handle_info( {'DOWN', MRef, _, _, _} ) when is_reference( MRef ) ->
  {noreply, #{}, #{ 'Down' => [MRef] }};

handle_info( {'EXIT', Pid, _Reason} ) when is_pid( Pid ) ->
  {noreply, #{}, #{ 'Exit' => [Pid] }};

handle_info( _Request, _NetState ) -> noreply.


%% @private
init( {WrkMod, WrkArgs, N} ) ->
  false = process_flag( trap_exit, true ),
  {ok, SupPid} = gruff_sup:start_link( WrkMod, WrkArgs ),
  GruffState = #gruff_state{ nwrk = N, sup_pid = SupPid },
  {ok, gen_pnet:new( ?MODULE, GruffState )}.


%% @private
terminate( _Reason, _NetState ) -> ok.


%% @private
trigger( 'Reply', {C, P}, _NetState ) -> gen_pnet:reply( C, {ok, P} ), drop;
trigger( _Place, _Token, _NetState )  -> pass.


%%====================================================================
%% Petri net structure callback functions
%%====================================================================

%% @private
place_lst() ->
  ['Down', 'Checkout', 'Cancel', 'Checkin', 'Exit', 'Reply',
   'Waiting', 'Busy', 'Idle', 'Unstarted'].


%% @private
trsn_lst() ->
  [down_busy, down_waiting, monitor, cancel_waiting, cancel_busy,
   free, alloc, exit_busy, exit_idle, start].


%% @private
init_marking( 'Unstarted', #gruff_state{ nwrk=N } ) -> lists:duplicate( N, t );
init_marking( _, _ )                                -> [].


%% @private
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


%% @private
is_enabled( down_busy,      #{ 'Down'    := [M], 'Busy'    := [{_, M, _}] } ) -> true;
is_enabled( down_waiting,   #{ 'Down'    := [M], 'Waiting' := [{_, _, M}] } ) -> true;
is_enabled( monitor,        _ )                                               -> true;
is_enabled( cancel_waiting, #{ 'Cancel'  := [R], 'Waiting' := [{_, R, _}] } ) -> true;
is_enabled( cancel_busy,    #{ 'Cancel'  := [R], 'Busy'    := [{R, _, _}] } ) -> true;
is_enabled( free,           #{ 'Checkin' := [P], 'Busy'    := [{_, _, P}] } ) -> true;
is_enabled( alloc,          _ )                                               -> true;
is_enabled( exit_busy,      #{ 'Exit'    := [P], 'Busy'    := [{_, _, P}] } ) -> true;
is_enabled( exit_idle,      #{ 'Exit'    := [P], 'Idle'    := [P] } )         -> true;
is_enabled( start,          _ )                                               -> true;
is_enabled( _,              _ )                                               -> false.


%% @private
fire( start, #{ 'Unstarted' := [t] }, #gruff_state{ sup_pid = SupPid } ) ->
  {ok, WrkPid} = supervisor:start_child( SupPid, [] ),
  true = link( WrkPid ),
  {produce, #{ 'Idle' => [WrkPid] }};

fire( exit_idle, #{ 'Exit' := [P], 'Idle' := [P] } ) ->
  {produce, #{ 'Unstarted' => [t] }};

fire( exit_busy, #{ 'Exit' := [P], 'Busy' := [_R, M, P] } ) ->
  true = demonitor( M ),
  {produce, #{ 'Unstarted' => [t] }};

fire( down_waiting, #{ 'Down' := [M], 'Waiting' := [{_C, _R, M}] } ) ->
  {produce, #{}};

fire( cancel_waiting, #{ 'Cancel' := [R], 'Waiting' := [{_C, R, M}] } ) ->
  true = demonitor( M ),
  {produce, #{}};

fire( monitor, #{ 'Checkout' := [{C, R}] } ) ->
  C = {ClientPid, _Tag},
  M = monitor( process, ClientPid ),
  {produce, #{ 'Waiting' => [{C, R, M}] }};

fire( down_busy, #{ 'Down' := [M], 'Busy' := [{_R, M, P}] } ) ->
  {produce, #{ 'Idle' => [P] }};

fire( alloc, #{'Waiting' := [{C, R, M}], 'Idle' := [P] } ) ->
  {produce, #{ 'Reply' => [{C, P}], 'Busy' => [{R, M, P}] }};

fire( cancel_busy, #{ 'Cancel' := [R], 'Busy' := [{R, M, P}] } ) ->
  true = demonitor( M ),
  {produce, #{ 'Idle' => [P] }};

fire( free, #{ 'Checkin' := [P], 'Busy' := [{_R, M, P}] } ) ->
  true = demonitor( M ),
  {produce, #{ 'Idle' => [P] }}.