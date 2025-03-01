%% -*- erlang -*-
%%
%% A basic worker pool manager for Erlang to showcase gen_pnet.
%%
%% Copyright 2017 Jörgen Brandt
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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @version 0.1.0
%% @copyright 2017 Jörgen Brandt
%%
%% @end
%% -------------------------------------------------------------------

-module(gruff).
-behavior(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2,
         trigger/3]).

-export([place_lst/0,
         trsn_lst/0,
         init_marking/2,
         preset/1,
         is_enabled/3,
         fire/3]).

-export([start_link/2, start_link/3,
         checkout/1, checkout/2,
         checkin/1,
         transaction/2, transaction/3,
         stop/1,
         get_pid/1]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("gen_pnet/include/gen_pnet.hrl").

%%====================================================================
%% Record definitions
%%====================================================================

-record(gruff_state, {nwrk :: pos_integer(), sup_pid :: pid()}).
-record(worker, {name :: name(), ref :: reference(), pid :: pid()}).

%%====================================================================
%% Type definitions
%%====================================================================

-type name() :: atom() |
                {atom(), atom()} |
                {global, _} |
                {via, atom(), _} |
                pid().

-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

-type result() :: {ok, pid()} |
                  ignore |
                  {error, _}.

%%====================================================================
%% Macro definitions
%%====================================================================

-define(TIMEOUT, 5000).

%%====================================================================
%% API functions
%%====================================================================


%% @doc Returns the Pid of a worker object.
-spec get_pid(#worker{}) -> pid().

get_pid(#worker{pid = WrkPid}) ->
    WrkPid.


%% @doc Starts an instance of a gruff worker pool.
%% @see start_link/4
-spec start_link({M, F, A}, N) -> result()
              when M :: atom(),
                   F :: atom(),
                   A :: [_],
                   N :: pos_integer().

start_link({M, F, A}, N)
  when is_atom(M),
       is_atom(F),
       is_list(A),
       is_integer(N),
       N > 0 ->
    gen_pnet:start_link(?MODULE, {{M, F, A}, N}, []).


%% @doc Starts an instance of a gruff worker pool registered as `ServerName'.
%%      The workers are specified in the `{M, F, A}' triple denoting the worker
%%      module `M', the start function `F', and the start function's argument
%%      list `A'. `N' is the number of workers to be maintained by this worker
%%      pool. Returns `{ok, Pid}' on success. Returns `ignore' or
%%      `{error, Reason}' otherwise.
-spec start_link(ServerName, {M, F, A}, N) -> result()
              when ServerName :: server_name(),
                   M :: atom(),
                   F :: atom(),
                   A :: [_],
                   N :: pos_integer().

start_link(ServerName, {M, F, A}, N)
  when is_tuple(ServerName),
       is_atom(M),
       is_atom(F),
       is_list(A),
       is_integer(N),
       N > 0 ->
    gen_pnet:start_link(ServerName, ?MODULE, {{M, F, A}, N}, []).


%% @doc Checks out a worker instance from the worker pool. Times out after five
%%      seconds.
%% @see checkout/2
-spec checkout(Name) -> Result
              when Name :: name(),
                   Result :: {ok, #worker{}} |
                             {error, _}.

checkout(Name) -> checkout(Name, ?TIMEOUT).


%% @doc Checks out a worker instance from the worker pool with an explicit time
%%      out interval. `Name' is the name of the gruff process instance created
%%      with `start_link/n'. The result is either `{ok, Pid}' or
%%      `{error, Reason}' where `Pid' is the process id of the successfully
%%      allocated worker instance. The function times out after `Timeout'
%%      milliseconds.
-spec checkout(Name, Timeout) -> Result
              when Name :: name(),
                   Timeout :: non_neg_integer(),
                   Result :: {ok, #worker{}} |
                             {error, _}.

checkout(Name, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    R = make_ref(),
    try
        {ok, WrkPid} = gen_pnet:call(Name, {checkout, R}, Timeout),
        {ok, #worker{name = Name, ref = R, pid = WrkPid}}
    catch
        _:Reason ->
            ok = gen_pnet:cast(Name, {cancel, R}),
            {error, Reason}
    end.


%% @doc Checks in a previously checked out worker instance. The `Name' argument
%%      identifies the gruff instance and the `WrkPid' argument is the process
%%      id of a worker instance, that has previously been allocated using
%%      `checkout/n'.
-spec checkin(#worker{}) -> ok.

checkin(#worker{name = Name, ref = R}) when is_reference(R) ->
    ok = gen_pnet:cast(Name, {checkin, R}).


%% @doc Checks out a worker, applies a given function to it, and checks it in
%%      again. Times out after five seconds.
%% @see transaction/3
-spec transaction(Name, Fun) -> {ok, _} | {error, _}
              when Name :: name(),
                   Fun :: fun((_) -> _).

transaction(Name, Fun) when is_function(Fun, 1) ->
    transaction(Name, Fun, ?TIMEOUT).


%% @doc Checks out a worker, applies a given function to it, and checks it in
%%      again applying an explicit timeout. The gruff instance identified by the
%%      `Name' argument is used to check out a worker and apply the unary
%%      function `Fun' to it. Afterwards, the worker instance is checked in
%%      again. On success, `transaction/3' returns `{ok, Value}' where `Value'
%%      is the return value of the given function `Fun'. If the function throws
%%      an exception or the checkout fails or times out then `{error, Reason}'
%%      is returned.
-spec transaction(Name, Fun, Timeout) -> {ok, _} | {error, _}
              when Name :: name(),
                   Fun :: fun((_) -> _),
                   Timeout :: non_neg_integer().

transaction(Name, Fun, Timeout)
  when is_function(Fun, 1), is_integer(Timeout), Timeout >= 0 ->
    case checkout(Name, Timeout) of
        {error, Reason} -> {error, Reason};
        {ok, Wrk} ->
            try
                {ok, Fun(get_pid(Wrk))}
            catch
                _:Reason -> {error, Reason}
            after
                ok = checkin(Wrk)
            end
    end.


%% @doc Stops the gruff instance under the given name.
-spec stop(Name) -> ok
              when Name :: name().

stop(Name) ->
    gen_pnet:stop(Name).


%%====================================================================
%% Actor interface callback functions
%%====================================================================


%% @private
code_change(_OldVsn, NetState, _Extra) -> {ok, NetState}.


%% @private
handle_call({checkout, R}, From, _NetState)
  when is_reference(R), is_tuple(From) ->
    {noreply, #{'Checkout' => [{From, R}]}};

handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.


%% @private
handle_cast({cancel, R}, _NetState) when is_reference(R) ->
    {noreply, #{'Cancel' => [R]}};

handle_cast({checkin, R}, _NetState) when is_reference(R) ->
    {noreply, #{'Checkin' => [R]}};

handle_cast(_Request, _NetState) -> noreply.


%% @private
handle_info({'DOWN', MRef, _, _, _}, _NetState) when is_reference(MRef) ->
    {noreply, #{'Down' => [MRef]}};

handle_info({'EXIT', Pid, _Reason}, _NetState) when is_pid(Pid) ->
    {noreply, #{'Exit' => [Pid]}};

handle_info(_Request, _NetState) -> noreply.


%% @private
init({{M, F, A}, N})
  when is_atom(M),
       is_atom(F),
       is_list(A),
       is_integer(N),
       N > 0 ->
    false = process_flag(trap_exit, true),
    {ok, SupPid} = gruff_sup:start_link({M, F, A}),
    #gruff_state{nwrk = N, sup_pid = SupPid}.


%% @private
terminate(_Reason, _NetState) -> ok.


%% @private
trigger('Reply', {C, P}, _NetState) -> gen_pnet:reply(C, {ok, P}), drop;
trigger(_Place, _Token, _NetState) -> pass.


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
init_marking('Unstarted', #gruff_state{nwrk = N}) -> lists:duplicate(N, t);
init_marking(_, _) -> [].


%% @private
preset(down_busy) -> ['Down', 'Busy'];
preset(down_waiting) -> ['Down', 'Waiting'];
preset(monitor) -> ['Checkout'];
preset(cancel_waiting) -> ['Cancel', 'Waiting'];
preset(cancel_busy) -> ['Cancel', 'Busy'];
preset(free) -> ['Checkin', 'Busy'];
preset(alloc) -> ['Waiting', 'Idle'];
preset(exit_busy) -> ['Exit', 'Busy'];
preset(exit_idle) -> ['Exit', 'Idle'];
preset(start) -> ['Unstarted'].


%% @private
is_enabled(down_busy, #{'Down' := [M], 'Busy' := [{_, M, _}]}, _) -> true;
is_enabled(down_waiting, #{'Down' := [M], 'Waiting' := [{_, _, M}]}, _) -> true;
is_enabled(monitor, _, _) -> true;
is_enabled(cancel_waiting, #{'Cancel' := [R], 'Waiting' := [{_, R, _}]}, _) -> true;
is_enabled(cancel_busy, #{'Cancel' := [R], 'Busy' := [{R, _, _}]}, _) -> true;
is_enabled(free, #{'Checkin' := [R], 'Busy' := [{R, _, _}]}, _) -> true;
is_enabled(alloc, _, _) -> true;
is_enabled(exit_busy, #{'Exit' := [P], 'Busy' := [{_, _, P}]}, _) -> true;
is_enabled(exit_idle, #{'Exit' := [P], 'Idle' := [P]}, _) -> true;
is_enabled(start, _, _) -> true;
is_enabled(_, _, _) -> false.


%% @private
fire(start, #{'Unstarted' := [t]}, #gruff_state{sup_pid = SupPid}) ->
    {ok, WrkPid} = supervisor:start_child(SupPid, []),
    true = link(WrkPid),
    {produce, #{'Idle' => [WrkPid]}};

fire(exit_idle, #{'Exit' := [P], 'Idle' := [P]}, _) ->
    {produce, #{'Unstarted' => [t]}};

fire(exit_busy, #{'Exit' := [P], 'Busy' := [{_, M, P}]}, _) ->
    true = demonitor(M),
    {produce, #{'Unstarted' => [t]}};

fire(down_waiting, #{'Down' := [M], 'Waiting' := [{_, _, M}]}, _) ->
    {produce, #{}};

fire(cancel_waiting, #{'Cancel' := [R], 'Waiting' := [{_, R, M}]}, _) ->
    true = demonitor(M),
    {produce, #{}};

fire(monitor, #{'Checkout' := [{C, R}]}, _) ->
    {ClientPid, _Tag} = C,
    M = monitor(process, ClientPid),
    {produce, #{'Waiting' => [{C, R, M}]}};

fire(down_busy, #{'Down' := [M], 'Busy' := [{_, M, P}]}, _) ->
    {produce, #{'Idle' => [P]}};

fire(alloc, #{'Waiting' := [{C, R, M}], 'Idle' := [P]}, _) ->
    {produce, #{'Reply' => [{C, P}], 'Busy' => [{R, M, P}]}};

fire(cancel_busy, #{'Cancel' := [R], 'Busy' := [{R, M, P}]}, _) ->
    true = demonitor(M),
    {produce, #{'Idle' => [P]}};

fire(free, #{'Checkin' := [R], 'Busy' := [{R, M, P}]}, _) ->
    true = demonitor(M),
    {produce, #{'Idle' => [P]}}.
