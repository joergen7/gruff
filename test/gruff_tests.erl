-module( gruff_tests ).

-include_lib( "eunit/include/eunit.hrl" ).

-define( GRUFF_NAME, gruff_test ).

%%====================================================================
%% Test definition
%%====================================================================

gruff_test_() ->
  {foreach,
   
   fun() -> ok end,

   fun( _ ) ->
     case whereis( ?GRUFF_NAME ) of
       undefined -> ok;
       Pid       -> gruff:stop( Pid )
     end
   end,

   [
    {<<"Golden path">>, fun golden_path/0}
   ]

  }.


%%====================================================================
%% Test implementation
%%====================================================================

golden_path() ->

  % start new gruff pool
  {ok, Pid} = new_gruff( 10 ),

  % all workers are either unstarted or idle
  #{ 'Unstarted' := Unstarted1, 'Idle' := Idle1 } = gen_pnet:marking( Pid ),
  ?assertEqual( 10, length( Unstarted1 )+length( Idle1 ) ),

  % checkout first worker
  {ok, _} = gruff:checkout( Pid ),

  % one worker is busy while the others are idle or unstarted
  #{ 'Unstarted' := Unstarted2,
     'Idle'      := Idle2,
     'Busy'      := Busy2 } = gen_pnet:marking( Pid ),
  ?assertEqual( 9, length( Unstarted2 )+length( Idle2 ) ),
  ?assertEqual( 1, length( Busy2 ) ),

  % checkout second worker
  {ok, Worker} = gruff:checkout( Pid ),

  % two workers are busy while the others are idle or unstarted
  #{ 'Unstarted' := Unstarted3,
     'Idle'      := Idle3,
     'Busy'      := Busy3 } = gen_pnet:marking( Pid ),
  ?assertEqual( 8, length( Unstarted3 )+length( Idle3 ) ),
  ?assertEqual( 2, length( Busy3 ) ),

  % checkin second worker
  ok = gruff:checkin( Pid, Worker ),

  % one worker is busy while the others are idle or unstarted
  #{ 'Unstarted' := Unstarted4,
     'Idle'      := Idle4,
     'Busy'      := Busy4,
     'Checkin'   := Checkin4 } = gen_pnet:marking( Pid ),
  ?assertEqual( 9, length( Unstarted4 )+length( Idle4 )+length( Checkin4 ) ),
  ?assertEqual( 1, length( Busy4 )-length( Checkin4 ) ),

  % stop gruff instance
  ok = gruff:stop( Pid ).




%%====================================================================
%% Helper functions
%%====================================================================

new_gruff( N ) ->
  gruff:start_link( {local, ?GRUFF_NAME}, {add_wrk, start_link, []}, N ).