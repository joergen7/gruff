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

-module( gruff_sup ).
-behaviour( supervisor ).

-export( [start_link/2] ).
-export( [init/1] ).


%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts an instance of a gruff supervisor. The argument `WrkMod' is the
%%      worker module name and the `WrkArgs' argument is handed to the worker
%%      process on startup.
start_link( WrkMod, WrkArgs ) when is_atom( WrkMod ) ->
    supervisor:start_link( ?MODULE, {WrkMod, WrkArgs} ).


%%====================================================================
%% Supervisor callback functions
%%====================================================================

%% @private
init( {WrkMod, WrkArgs} ) when is_atom( WrkMod ) ->

    SupFlags = #{
                  strategy  => simple_one_for_one,
                  intensity => 0,
                  period    => 1
                },

    ChildSpec = #{
                   id       => undefined,
                   start    => {WrkMod, start_link, [WrkArgs]},
                   restart  => temporary,
                   shutdown => 5000,
                   type     => worker,
                   modules  => [WrkMod]
                 },

    {ok, {SupFlags, [ChildSpec]}}.
