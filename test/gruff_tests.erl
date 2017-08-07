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
    {<<"Golden path">>, fun golden_path/0},
    {<<"Checkout all and checkin all">>, fun checkout_all_checkin_all/0},
    {<<"Dead worker is restarted">>, fun dead_worker_is_restarted/0}
   ]

  }.


%%====================================================================
%% Test implementation
%%====================================================================

golden_path() ->

  % start new gruff instance
  {ok, Pid} = new_gruff( 10 ),

  % all workers are either unstarted or idle
  #{ 'Unstarted' := Unstarted1,
     'Idle'      := Idle1,
     'Busy'      := Busy1 } = gen_pnet:marking( Pid ),
  check_invariant( 10, Unstarted1, Idle1, Busy1 ),
  ?assertEqual( 10, length( Unstarted1 )+length( Idle1 ) ),

  % checkout first worker
  {ok, _} = gruff:checkout( Pid ),

  % one worker is busy while the others are idle or unstarted
  #{ 'Unstarted' := Unstarted2,
     'Idle'      := Idle2,
     'Busy'      := Busy2 } = gen_pnet:marking( Pid ),
  check_invariant( 10, Unstarted2, Idle2, Busy2 ),
  ?assertEqual( 9, length( Unstarted2 )+length( Idle2 ) ),
  ?assertEqual( 1, length( Busy2 ) ),

  % checkout second worker
  {ok, Worker} = gruff:checkout( Pid ),

  % two workers are busy while the others are idle or unstarted
  #{ 'Unstarted' := Unstarted3,
     'Idle'      := Idle3,
     'Busy'      := Busy3 } = gen_pnet:marking( Pid ),
  check_invariant( 10, Unstarted3, Idle3, Busy3 ),
  ?assertEqual( 8, length( Unstarted3 )+length( Idle3 ) ),
  ?assertEqual( 2, length( Busy3 ) ),

  % checkin second worker
  ok = gruff:checkin( Pid, Worker ),

  % one worker is busy while the others are idle or unstarted
  #{ 'Unstarted' := Unstarted4,
     'Idle'      := Idle4,
     'Busy'      := Busy4,
     'Checkin'   := Checkin4 } = gen_pnet:marking( Pid ),
  check_invariant( 10, Unstarted4, Idle4, Busy4 ),
  ?assertEqual( 9, length( Unstarted4 )+length( Idle4 )+length( Checkin4 ) ),
  ?assertEqual( 1, length( Busy4 )-length( Checkin4 ) ),

  % stop gruff instance
  ok = gruff:stop( Pid ).



checkout_all_checkin_all() ->

  % start new gruff instance
  {ok, Pid} = new_gruff( 7 ),

  % checkout all the workers
  Workers = [W || {ok, W} <- [gruff:checkout( Pid ) || _ <- lists:seq( 1, 7 )]],

  % all workers should be busy and none should be idle
  #{ 'Idle'      := Idle1,
     'Busy'      := Busy1,
     'Unstarted' := Unstarted1 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted1, Idle1, Busy1 ),
  ?assertEqual( 7, length( Busy1 ) ),
  ?assertEqual( 0, length( Idle1 ) ),

  [A, B, C, D, E, F, G] = Workers,

  % checkin the first two workers
  gruff:checkin( Pid, A ),
  gruff:checkin( Pid, B ),

  % two workers should be checked in
  #{ 'Idle'      := Idle2,
     'Busy'      := Busy2,
     'Checkin'   := Checkin2,
     'Unstarted' := Unstarted2 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted2, Idle2, Busy2 ),
  ?assertEqual( 2, length( Idle2 )+length( Checkin2 ) ),
  ?assertEqual( 5, length( Busy2 )-length( Checkin2 ) ),

  % checkin the next two workers
  gruff:checkin( Pid, C ),
  gruff:checkin( Pid, D ),

  % four workers should be checked in
  #{ 'Idle'      := Idle3,
     'Busy'      := Busy3,
     'Checkin'   := Checkin3,
     'Unstarted' := Unstarted3 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted3, Idle3, Busy3 ),
  ?assertEqual( 4, length( Idle3 )+length( Checkin3 ) ),
  ?assertEqual( 3, length( Busy3 )-length( Checkin3 ) ),

  % checkin all but one worker
  gruff:checkin( Pid, E ),
  gruff:checkin( Pid, F ),

  % six workers should be checked in
  #{ 'Idle'      := Idle4,
     'Busy'      := Busy4,
     'Checkin'   := Checkin4,
     'Unstarted' := Unstarted4 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted4, Idle4, Busy4 ),
  ?assertEqual( 6, length( Idle4 )+length( Checkin4 ) ),
  ?assertEqual( 1, length( Busy4 )-length( Checkin4 ) ),

  % checkin last worker
  gruff:checkin( Pid, G ),

  % all seven workers should be checked in
  #{ 'Idle'      := Idle5,
     'Busy'      := Busy5,
     'Checkin'   := Checkin5,
     'Unstarted' := Unstarted5 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted5, Idle5, Busy5 ),
  ?assertEqual( 7, length( Idle5 )+length( Checkin5 ) ),
  ?assertEqual( 0, length( Busy5 )-length( Checkin5 ) ),

  % stop gruff instance
  ok = gruff:stop( Pid ).



dead_worker_is_restarted() ->

  % start new gruff instance
  {ok, Pid} = new_gruff( 7 ),

  % checkout worker
  {ok, Worker} = gruff:checkout( Pid ),

  % kill worker
  kill_worker( Worker ).

%%====================================================================
%% Helper functions
%%====================================================================

new_gruff( N ) ->
  gruff:start_link( {local, ?GRUFF_NAME}, {add_wrk, start_link, []}, N ).


check_invariant( N, Unstarted, Idle, Busy ) ->
  ?assertEqual( N, length( Unstarted )+length( Idle )+length( Busy ) ).


kill_worker( Pid ) when is_pid( Pid ) ->
  erlang:monitor( process, Pid ),
  gen_server:stop( Pid ),
  receive
    {'DOWN', _, process, Pid, _} -> ok
  end.
