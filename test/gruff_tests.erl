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
    {<<"golden path">>,
     fun golden_path/0},

    {<<"checkout all and checkin all">>,
     fun checkout_all_checkin_all/0},

    {<<"checked out dead worker restarts">>,
     fun checked_out_dead_worker_restarts/0},

    {<<"checkout from all busy makes client wait">>,
     fun checkout_from_all_busy_makes_client_wait/0},

    {<<"dead process owner frees worker">>,
     fun dead_process_owner_frees_worker/0}

    % {<<"idle dead worker restarts">>,
    % fun idle_dead_worker_restarts/0},
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



checked_out_dead_worker_restarts() ->

  % start new gruff instance
  {ok, Pid} = new_gruff( 7 ),

  % checkout worker
  {ok, Worker} = gruff:checkout( Pid ),

  % kill worker
  kill_worker( Worker ),

  % the killed worker should be restarted
  #{ 'Unstarted' := Unstarted1,
     'Idle'      := Idle1,
     'Busy'      := Busy1,
     'Exit'      := Exit1 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted1, Idle1, Busy1 ),
  ?assertEqual( 7, length( Idle1 )+length( Unstarted1 )+length( Exit1 ) ),
  ?assertEqual( 0, length( Busy1 )-length( Exit1 ) ),

  % checkout all seven workers
  Wrks = [W || {ok, W} <- [gruff:checkout( Pid ) || _ <- lists:seq( 1, 7 )]],
  [A, B, C|_] = Wrks,

  % seven workers should be in busy state
  #{ 'Unstarted' := Unstarted2,
     'Idle'      := Idle2,
     'Busy'      := Busy2 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted2, Idle2, Busy2 ),
  ?assertEqual( 7, length( Busy2 ) ),
  ?assertEqual( 0, length( Unstarted2 )+length( Idle2 ) ),

  % kill first worker
  kill_worker( A ),

  % the killed worker should be restarted
  #{ 'Unstarted' := Unstarted3,
     'Idle'      := Idle3,
     'Busy'      := Busy3,
     'Exit'      := Exit3 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted3, Idle3, Busy3 ),
  ?assertEqual( 1, length( Idle3 )+length( Unstarted3 )+length( Exit3 ) ),
  ?assertEqual( 6, length( Busy3 )-length( Exit3 ) ),

  % kill the other two workers
  kill_worker( B ),
  kill_worker( C ),

  % the killed worker should be restarted
  #{ 'Unstarted' := Unstarted4,
     'Idle'      := Idle4,
     'Busy'      := Busy4,
     'Exit'      := Exit4 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted4, Idle4, Busy4 ),
  ?assertEqual( 3, length( Idle4 )+length( Unstarted4 )+length( Exit4 ) ),
  ?assertEqual( 4, length( Busy4 )-length( Exit4 ) ),
  
  % stop gruff instance
  ok = gruff:stop( Pid ).




checkout_from_all_busy_makes_client_wait() ->

  % start new gruff instance
  {ok, Pid} = new_gruff( 7 ),

  % checkout single worker
  {ok, Worker} = gruff:checkout( Pid ),

  % kill the checked out worker
  kill_worker( Worker ),

  % the killed worker should be restarted
  #{ 'Unstarted' := Unstarted1,
     'Idle'      := Idle1,
     'Busy'      := Busy1,
     'Exit'      := Exit1 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted1, Idle1, Busy1 ),
  ?assertEqual( 7, length( Idle1 )+length( Unstarted1 )+length( Exit1 ) ),
  ?assertEqual( 0, length( Busy1 )-length( Exit1 ) ),

  % checkout all seven workers
  Wrks = [W || {ok, W} <- [gruff:checkout( Pid ) || _ <- lists:seq( 1, 7 )]],
  [A, B|_] = Wrks,

  % seven workers should be in busy state
  #{ 'Unstarted' := Unstarted2,
     'Idle'      := Idle2,
     'Busy'      := Busy2 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted2, Idle2, Busy2 ),
  ?assertEqual( 7, length( Busy2 ) ),
  ?assertEqual( 0, length( Unstarted2 )+length( Idle2 ) ),

  % start a process that asks for a worker when none are available
  Self = self(),
  spawn_link( fun() ->
    {ok, _} = gruff:checkout( Pid ),
    Self ! got_worker,
    timer:sleep( 5000 )
  end ),

  % the new process should be waiting for a worker so a got_worker message
  % should not be received
  receive
    got_worker -> ?assert( false )
  after
    500 -> ?assert( true )
  end,

  % the waiting process should reside on either the Checkout or Waiting place
  #{ 'Unstarted' := Unstarted3,
     'Idle'      := Idle3,
     'Busy'      := Busy3,
     'Waiting'   := Waiting3 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted3, Idle3, Busy3 ),
  ?assertEqual( 7, length( Busy3 ) ),
  ?assertEqual( 0, length( Unstarted3 )+length( Idle3 ) ),
  ?assertEqual( 1, length( Waiting3 ) ),

  % kill one of the workers
  kill_worker( A ),

  % the killed worker should be restarted and allocated to the process
  #{ 'Unstarted' := Unstarted4,
     'Idle'      := Idle4,
     'Busy'      := Busy4 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted4, Idle4, Busy4 ),

  % the process should get the new worker right away since it is still waiting
  % for a reply
  receive
    got_worker -> ?assert( true )
  after
    1000 ->
      io:format( "~p~n", [gen_pnet:marking( Pid )] ),
      ?assert( false )
  end,

  #{ 'Unstarted' := Unstarted5,
     'Idle'      := Idle5,
     'Busy'      := Busy5 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted5, Idle5, Busy5 ),
  ?assertEqual( 7, length( Busy5 ) ),
  ?assertEqual( 0, length( Unstarted5 )+length( Idle5 ) ),

  % kill another one of the workers
  kill_worker( B ),

  % the killed worker should be restarted
  #{ 'Unstarted' := Unstarted6,
     'Idle'      := Idle6,
     'Busy'      := Busy6,
     'Exit'      := Exit6 } = gen_pnet:marking( Pid ),
  check_invariant( 7, Unstarted6, Idle6, Busy6 ),
  ?assertEqual( 1, length( Idle6 )+length( Unstarted6 )+length( Exit6 ) ),
  ?assertEqual( 6, length( Busy6 )-length( Exit6 ) ),

  % stop gruff instance
  ok = gruff:stop( Pid ).




dead_process_owner_frees_worker() ->

  % start new gruff instance
  {ok, Pid} = new_gruff( 10 ),

  % start a process that checks out a worker and dies without giving it back
  spawn( fun() ->
    {ok, _} = gruff:checkout( Pid ),
    exit( normal )
  end ),

  % wait half a second
  timer:sleep( 500 ),
  
  % after some time, the gruff instance should just sit there with ten idle
  % workers
  #{ 'Unstarted' := Unstarted1,
     'Idle'      := Idle1,
     'Busy'      := Busy1,
     'Down'      := Down1 } = gen_pnet:marking( Pid ),
  check_invariant( 10, Unstarted1, Idle1, Busy1 ),
  ?assertEqual( 10, length( Idle1 ) ),
  ?assertEqual( 0, length( Down1 ) ),

  % stop gruff instance
  ok = gruff:stop( Pid ).







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
