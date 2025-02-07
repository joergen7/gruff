-module(add_wrk).
-behavior(gen_server).

-export([start_link/0]).

-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).


start_link() -> gen_server:start_link(?MODULE, [], []).


code_change(_OldVsn, State, _Extra) -> {ok, State}.


handle_call({add, A, B}, _From, State) -> {reply, A + B, State}.


handle_cast(_Request, State) -> {noreply, State}.


handle_info(_Info, State) -> {noreply, State}.


init(_Args) -> {ok, []}.


terminate(_Reason, _State) -> ok.
