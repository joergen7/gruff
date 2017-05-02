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
%% @author Jörgen Brandt <joergen.brandt@onlinehome.de>
%% @version 0.1.0
%% @copyright 2017 Jörgen Brandt
%%
%% @end
%% -------------------------------------------------------------------

-module( gruff_sup ).
-behaviour( supervisor ).

-export( [start_link/1] ).
-export( [init/1] ).


%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an instance of a gruff supervisor. The `{M, F, A}' argument
%%      triple specifies how the worker processes are started. Herein, `M'
%%      denotes the worker's module name, `F' is the start function, and `A' is
%%      the argument list handed to the worker process on startup.
-spec start_link( {M, F, A} ) -> {ok, pid()} | ignore | {error, _}
when M :: atom(),
     F :: atom(),
     A :: [_].

start_link( {M, F, A} ) when is_atom( M ), is_atom( F ), is_list( A ) ->
    supervisor:start_link( ?MODULE, {M, F, A} ).


%%====================================================================
%% Supervisor callback functions
%%====================================================================

%% @private
init( {M, F, A} ) when is_atom( M ), is_atom( F ), is_list( A ) ->

    SupFlags = #{
                  strategy  => simple_one_for_one,
                  intensity => 0,
                  period    => 1
                },

    ChildSpec = #{
                   id       => undefined,
                   start    => {M, F, A},
                   restart  => temporary,
                   shutdown => 5000,
                   type     => worker,
                   modules  => [M]
                 },

    {ok, {SupFlags, [ChildSpec]}}.
