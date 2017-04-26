%% -*- erlang -*-
%%
%% A basic worker pool manager for Erlang to demonstrate the expressive power of
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

-export( [start_link/3, start_link/4, checkout/1, checkout/2, checkin/2,
          transaction/2, transaction/3] ).

%%====================================================================
%% Includes
%%====================================================================

-include_lib( "gen_pnet/include/gen_pnet.hrl" ).

%%====================================================================
%% Record definitions
%%====================================================================

-record( gruff_state, {nwrk, sup_pid} ).

%%====================================================================
%% Type definitions
%%====================================================================

-type pool() :: atom()
              | {atom(), atom()}
              | {global, _}
              | {via, atom(), _}
              | pid().

-type server_name() :: {local, atom()}
                     | {global, atom()}
                     | {via, atom(), _}.

-type result() :: {ok, pid()}
                | ignore
                | {error, _}.

%%====================================================================
%% Macro definitions
%%====================================================================

-define( TIMEOUT, 5000 ).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an instance of a gruff worker pool.
%% @see start_link/4
-spec start_link( WrkMod, WrkArgs, N ) -> result()
when WrkMod  :: atom(),
     WrkArgs :: _,
     N       :: pos_integer().

start_link( WrkMod, WrkArgs, N )
when is_atom( WrkMod ), is_integer( N ), N > 0 ->
  gen_pnet:start_link( ?MODULE, {WrkMod, WrkArgs, N}, [] ).

%% @doc Starts an instance of a gruff worker pool registered as `ServerName'.
%%      The workers are specified in the module given as the `WrkMod' argument.
%%      This module is expected to implement the `gruff_wrk' behavior. On
%%      startup the worker process is given `WrkArgs' as an argument. `N' is the
%%      number of workers to be maintained by this worker pool. Returns
%%      `{ok, Pid}' on success. Returns `ignore' or `{error, Reason} otherwise.
-spec start_link( ServerName, WrkMod, WrkArgs, N ) -> result()
when ServerName :: server_name(),
     WrkMod     :: atom(),
     WrkArgs    :: _,
     N          :: pos_integer().

start_link( ServerName, WrkMod, WrkArgs, N )
when is_atom( WrkMod ), is_integer( N ), N > 0 ->
  gen_pnet:start_link( ServerName, ?MODULE, {WrkMod, WrkArgs, N}, [] ).


%% @doc Checks out a worker instance from the worker pool. Times out after five
%%      seconds.
%% @see checkout/2
-spec checkout( Pool ) -> {ok, pid()} | {error, _}
when Pool :: pool().

checkout( Pool ) -> checkout( Pool, ?TIMEOUT ).


%% @doc Checks out a worker instance from the worker pool with an explicit time
%%      out interval. `Pool' is the name of the gruff process instance created
%%      with `start_link/n'. The result is either `{ok, Pid}' or
%%      `{error, Reason}' where `Pid' is the process id of the successfully
%%      allocated worker instance. The function times out after `Timeout'
%%      milliseconds.
-spec checkout( Pool, Timeout ) -> {ok, pid()} | {error, _}
when Pool    :: pool(),
     Timeout :: non_neg_integer().

checkout( Pool, Timeout ) when is_integer( Timeout ), Timeout >= 0 ->
  R = make_ref(),
  try
    gen_pnet:call( Pool, {checkout, R}, Timeout )
  catch
    _:Reason ->
      ok = gen_pnet:cast( Pool, {cancel, R} ),
      {error, Reason}
  end.

%% @doc Checks in a previously checked out worker instance. The `Pool' argument
%%      identifies the gruff instance and the `WrkPid' argument is the process
%%      id of a worker instance, that has previously been allocated using
%%      `checkout/n'.
-spec checkin( Pool, WrkPid ) -> ok
when Pool   :: pool(),
     WrkPid :: pid().

checkin( Pool, WrkPid ) when is_pid( WrkPid ) ->
  ok = gen_pnet:cast( Pool, {checkin, WrkPid} ).

%% @doc Checks out a worker, applies a given function to it, and checks it in
%%      again. Times out after five seconds.
%% @see transaction/3
-spec transaction( Pool, Fun ) -> {ok, _} | {error, _}
when Pool :: pool(),
     Fun  :: fun( ( _ ) -> _ ).

transaction( Pool, Fun ) when is_function( Fun, 1 ) ->
  transaction( Pool, Fun, ?TIMEOUT ).


%% @doc Checks out a worker, applies a given function to it, and checks it in
%%      again applying an explicit timeout. The gruff instance identified by the
%%      `Pool' argument is used to check out a worker and apply the unary
%%      function `Fun' to it. Afterwards, the worker instance is checked in
%%      again. On success, `transaction/3' returns `{ok, Value}' where `Value'
%%      is the return value of the given function `Fun'. If the function throws
%%      an exception or the checkout fails or times out then `{error, Reason}'
%%      is returned.
-spec transaction( Pool, Fun, Timeout ) -> {ok, _} | {error, _}
when Pool    :: pool(),
     Fun     :: fun( ( _ ) -> _ ),
     Timeout :: non_neg_integer().

transaction( Pool, Fun, Timeout )
when is_function( Fun, 1 ), is_integer( Timeout ), Timeout >= 0 ->
  case checkout( Pool, Timeout ) of
    {error, Reason} -> {error, Reason};
    {ok, WrkPid}    ->
      try
        {ok, Fun( WrkPid )}
      catch
        _:Reason -> {error, Reason}
      after
        ok = checkin( Pool, WrkPid )
      end
  end.

%%====================================================================
%% Actor interface callback functions
%%====================================================================

%% @private
code_change( _OldVsn, NetState, _Extra ) -> {ok, NetState}.


%% @private
handle_call( {checkout, R}, From, _NetState )
when is_reference( R ), is_tuple( From ) ->
  {noreply, #{}, #{ 'Checkout' => [{From, R}] }};

handle_call( _Request, _From, _NetState ) ->
  {reply, {error, bad_msg}}.


%% @private
handle_cast( {cancel, R}, _NetState ) when is_reference( R ) ->
  {noreply, #{}, #{ 'Cancel' => [R]}};

handle_cast( {checkin, P}, _NetState ) when is_pid( P ) ->
  {noreply, #{}, #{ 'Checkin' => [P] }};

handle_cast( _Request, _NetState ) -> noreply.


%% @private
handle_info( {'DOWN', MRef, _, _, _}, _NetState ) when is_reference( MRef ) ->
  {noreply, #{}, #{ 'Down' => [MRef] }};

handle_info( {'EXIT', Pid, _Reason}, _NetState ) when is_pid( Pid ) ->
  {noreply, #{}, #{ 'Exit' => [Pid] }};

handle_info( _Request, _NetState ) -> noreply.


%% @private
init( {WrkMod, WrkArgs, N} ) ->
  false = process_flag( trap_exit, true ),
  {ok, SupPid} = gruff_sup:start_link( WrkMod, WrkArgs ),
  GruffState = #gruff_state{ nwrk=N, sup_pid=SupPid },
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
fire( start, #{ 'Unstarted' := [t] }, #gruff_state{ sup_pid=SupPid } ) ->
  {ok, WrkPid} = supervisor:start_child( SupPid, [] ),
  true = link( WrkPid ),
  {produce, #{ 'Idle' => [WrkPid] }};

fire( exit_idle, #{ 'Exit' := [P], 'Idle' := [P] }, _ ) ->
  {produce, #{ 'Unstarted' => [t] }};

fire( exit_busy, #{ 'Exit' := [P], 'Busy' := [_, M, P] }, _ ) ->
  true = demonitor( M ),
  {produce, #{ 'Unstarted' => [t] }};

fire( down_waiting, #{ 'Down' := [M], 'Waiting' := [{_, _, M}] }, _ ) ->
  {produce, #{}};

fire( cancel_waiting, #{ 'Cancel' := [R], 'Waiting' := [{_, R, M}] }, _ ) ->
  true = demonitor( M ),
  {produce, #{}};

fire( monitor, #{ 'Checkout' := [{C, R}] }, _ ) ->
  {ClientPid, _Tag} = C,
  M = monitor( process, ClientPid ),
  {produce, #{ 'Waiting' => [{C, R, M}] }};

fire( down_busy, #{ 'Down' := [M], 'Busy' := [{_, M, P}] }, _ ) ->
  {produce, #{ 'Idle' => [P] }};

fire( alloc, #{'Waiting' := [{C, R, M}], 'Idle' := [P] }, _ ) ->
  {produce, #{ 'Reply' => [{C, P}], 'Busy' => [{R, M, P}] }};

fire( cancel_busy, #{ 'Cancel' := [R], 'Busy' := [{R, M, P}] }, _ ) ->
  true = demonitor( M ),
  {produce, #{ 'Idle' => [P] }};

fire( free, #{ 'Checkin' := [P], 'Busy' := [{_, M, P}] }, _ ) ->
  true = demonitor( M ),
  {produce, #{ 'Idle' => [P] }}.
