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


