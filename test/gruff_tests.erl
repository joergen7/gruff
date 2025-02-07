-module(gruff_tests).

-include_lib("eunit/include/eunit.hrl").

-define(GRUFF_NAME, gruff_test).

%%====================================================================
%% Test definition
%%====================================================================


gruff_test_() ->
    {foreach,

     fun() -> ok end,

     fun(_) ->
             case whereis(?GRUFF_NAME) of
                 undefined -> ok;
                 Pid -> gruff:stop(Pid)
             end
     end,

     [{<<"golden path">>,
       fun golden_path/0},

      {<<"checkout 7/10 and check back in">>,
       fun checkout_seven_of_ten_and_check_back_in/0},

      {<<"checkout from all busy blocks client">>,
       fun checkout_from_all_busy_blocks_client/0},

      {<<"checked out dead worker restarts">>,
       fun checked_out_dead_worker_restarts/0},

      {<<"restarted dead worker is allocated to waiting client">>,
       fun restarted_dead_worker_is_allocated_to_waiting_client/0},

      {<<"workers are reused">>,
       fun workers_are_reused/0},

      {<<"dead process owner frees worker">>,
       fun dead_process_owner_frees_worker/0},

      {<<"exception in transaction is handled">>,
       fun exception_in_transaction_is_handled/0},

      {<<"checkout times out if no workers available">>,
       fun checkout_times_out_if_no_workers_available/0}

      % {<<"idle dead worker restarts">>,
      % fun idle_dead_worker_restarts/0},
     ]}.


%%====================================================================
%% Test implementation
%%====================================================================


golden_path() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(10),

    % all workers either idle or unstarted
    check_all_idle_or_unstarted(Pid, 10),

    % checkout first worker
    {ok, _} = gruff:checkout(Pid),

    % one worker is busy while the others are idle or unstarted
    check_busy(Pid, 10, 1),

    % checkout second worker
    {ok, Worker} = gruff:checkout(Pid),

    % two workers are busy while the others are idle or unstarted
    check_busy(Pid, 10, 2),

    % checkin second worker
    ok = gruff:checkin(Worker),
    timer:sleep(250),

    % one worker is busy while the others are idle or unstarted
    check_busy(Pid, 10, 1),

    % stop gruff instance
    ok = gruff:stop(Pid).


checkout_seven_of_ten_and_check_back_in() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(10),
    check_all_idle_or_unstarted(Pid, 10),

    % checkout seven the workers
    Workers = [ W || {ok, W} <- [ gruff:checkout(Pid) || _ <- lists:seq(1, 7) ] ],

    % all workers should be busy and none should be idle
    check_busy(Pid, 10, 7),

    [A, B, C, D, E, F, G] = Workers,

    % checkin the first two workers
    gruff:checkin(A),
    gruff:checkin(B),
    timer:sleep(250),

    % two workers should be checked in
    check_busy(Pid, 10, 5),

    % checkin the next two workers
    gruff:checkin(C),
    gruff:checkin(D),
    timer:sleep(250),

    % four workers should be checked in
    check_busy(Pid, 10, 3),

    % checkin all but one worker
    gruff:checkin(E),
    gruff:checkin(F),
    timer:sleep(250),

    % six workers should be checked in
    check_busy(Pid, 10, 1),

    % checkin last worker
    gruff:checkin(G),
    timer:sleep(250),

    % all seven workers should be checked in
    check_all_idle_or_unstarted(Pid, 10),

    % stop gruff instance
    ok = gruff:stop(Pid).


checkout_from_all_busy_blocks_client() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(7),
    check_all_idle_or_unstarted(Pid, 7),

    % checkout all seven workers
    Wrks = [ W || {ok, W} <- [ gruff:checkout(Pid) || _ <- lists:seq(1, 7) ] ],
    [A, B, C, D, E, F, G] = Wrks,

    % seven workers should be in busy state
    check_all_busy(Pid, 7),

    % start a process that asks for a worker when none are available
    Self = self(),
    spawn_link(fun() ->
                       {ok, Wrk} = gruff:checkout(Pid),
                       gruff:checkin(Wrk),
                       Self ! got_worker
               end),

    % the new process should be waiting for a worker so a got_worker message
    % should not be received
    receive
        got_worker -> ?assert(false)
    after
        500 ->
            ?assert(true)
    end,

    % the waiting process should reside on the Waiting place
    check_all_busy_one_waiting(Pid, 7),

    % checkin first two workers
    gruff:checkin(A),
    gruff:checkin(B),

    % the process should get the new worker right away since it is still waiting
    % for a reply
    receive
        got_worker -> ?assert(true)
    after
        500 ->
            ?assert(false)
    end,
    timer:sleep(250),

    % five workers should still be busy
    check_busy(Pid, 7, 5),

    % checkin next two workers
    gruff:checkin(C),
    gruff:checkin(D),
    timer:sleep(250),

    % three workers should still be busy
    check_busy(Pid, 7, 3),

    % checkin another two workers
    gruff:checkin(E),
    gruff:checkin(F),
    timer:sleep(250),

    % one worker should still be busy
    check_busy(Pid, 7, 1),

    % checkin last two workers
    gruff:checkin(G),
    timer:sleep(250),

    % one worker should still be busy
    check_all_idle_or_unstarted(Pid, 7),

    % stop gruff instance
    ok = gruff:stop(Pid).


checked_out_dead_worker_restarts() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(7),
    check_all_idle_or_unstarted(Pid, 7),

    % checkout worker
    {ok, Worker} = gruff:checkout(Pid),

    % one worker is busy
    check_busy(Pid, 7, 1),

    % kill worker
    kill_worker(Worker),
    timer:sleep(250),

    % the killed worker should be restarted
    check_all_idle_or_unstarted(Pid, 7),

    % checkout all seven workers
    Wrks = [ W || {ok, W} <- [ gruff:checkout(Pid) || _ <- lists:seq(1, 7) ] ],
    [A, B, C | _] = Wrks,

    % seven workers should be in busy state
    check_all_busy(Pid, 7),

    % kill first worker
    kill_worker(A),
    timer:sleep(250),

    % the killed worker should be restarted
    check_busy(Pid, 7, 6),

    % kill the other two workers
    kill_worker(B),
    kill_worker(C),
    timer:sleep(250),

    % the killed worker should be restarted
    check_busy(Pid, 7, 4),

    % stop gruff instance
    ok = gruff:stop(Pid).


restarted_dead_worker_is_allocated_to_waiting_client() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(7),
    check_all_idle_or_unstarted(Pid, 7),

    % checkout single worker
    {ok, Worker} = gruff:checkout(Pid),

    % one worker is busy
    check_busy(Pid, 7, 1),

    % kill the checked out worker
    kill_worker(Worker),
    timer:sleep(250),

    % the killed worker should be restarted
    check_all_idle_or_unstarted(Pid, 7),

    % checkout all seven workers
    Wrks = [ W || {ok, W} <- [ gruff:checkout(Pid) || _ <- lists:seq(1, 7) ] ],
    [A, B | _] = Wrks,

    % seven workers should be in busy state
    check_all_busy(Pid, 7),

    % start a process that asks for a worker when none are available
    Self = self(),
    P = spawn_link(fun() ->
                           {ok, _} = gruff:checkout(Pid),
                           Self ! got_worker,
                           receive ok -> ok end
                   end),

    % the new process should be waiting for a worker so a got_worker message
    % should not be received
    receive
        got_worker -> ?assert(false)
    after
        500 ->
            ?assert(true)
    end,

    % the waiting process should reside on the Waiting place
    check_all_busy_one_waiting(Pid, 7),

    % kill one of the workers
    kill_worker(A),

    % the process should get the new worker right away since it is still waiting
    % for a reply
    receive
        got_worker -> ?assert(true)
    after
        1000 ->
            ?assert(false)
    end,

    check_all_busy(Pid, 7),

    % kill another one of the workers
    kill_worker(B),
    timer:sleep(250),

    % the killed worker should be restarted
    check_busy(Pid, 7, 6),

    P ! ok,

    % stop gruff instance
    ok = gruff:stop(Pid).


workers_are_reused() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(5),
    check_all_idle_or_unstarted(Pid, 5),

    % checkout seven the workers
    Workers = [ W || {ok, W} <- [ gruff:checkout(Pid) || _ <- lists:seq(1, 5) ] ],

    % all workers should be busy and none should be idle
    check_all_busy(Pid, 5),

    [A1 | _] = Workers,

    % checkin the worker
    gruff:checkin(A1),
    timer:sleep(250),

    check_busy(Pid, 5, 4),

    % checkout the worker again
    {ok, A2} = gruff:checkout(Pid),

    check_all_busy(Pid, 5),

    ?assertEqual(gruff:get_pid(A1), gruff:get_pid(A2)),

    % stop gruff instance
    ok = gruff:stop(Pid).


dead_process_owner_frees_worker() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(10),
    check_all_idle_or_unstarted(Pid, 10),

    % start a process that checks out a worker and dies without giving it back
    spawn_link(fun() ->
                       {ok, _} = gruff:checkout(Pid)
               end),

    % wait half a second
    timer:sleep(250),

    % after some time, the gruff instance should just sit there with ten idle
    % workers
    check_all_idle_or_unstarted(Pid, 10),

    % stop gruff instance
    ok = gruff:stop(Pid).


exception_in_transaction_is_handled() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(2),
    check_all_idle_or_unstarted(Pid, 2),

    Tx = fun(WorkerPid) ->
                 ?assert(is_pid(WorkerPid)),
                 check_busy(Pid, 2, 1),
                 throw(it_on_the_ground)
         end,

    Result = gruff:transaction(Pid, Tx),
    ?assertEqual({error, it_on_the_ground}, Result),
    timer:sleep(250),

    check_all_idle_or_unstarted(Pid, 2),

    % stop gruff instance
    ok = gruff:stop(Pid).


checkout_times_out_if_no_workers_available() ->

    % start new gruff instance
    {ok, Pid} = new_gruff(1),
    check_all_idle_or_unstarted(Pid, 1),

    Self = self(),

    F = fun() ->
                {ok, _} = gruff:checkout(Pid),
                {error, {timeout, _}} = gruff:checkout(Pid, 250),
                Self ! ok,
                receive ok -> ok end
        end,

    P = spawn_link(F),
    receive ok -> ok end,
    check_all_busy(Pid, 1),

    P ! ok,

    timer:sleep(250),
    check_all_idle_or_unstarted(Pid, 1),

    % stop gruff instance
    ok = gruff:stop(Pid).


%%====================================================================
%% Helper functions
%%====================================================================


new_gruff(N) ->
    gruff:start_link({local, ?GRUFF_NAME}, {add_wrk, start_link, []}, N).


kill_worker(Wrk) when is_tuple(Wrk) ->
    Pid = gruff:get_pid(Wrk),
    erlang:monitor(process, Pid),
    gen_server:stop(Pid),
    receive
        {'DOWN', _, process, Pid, _} -> ok
    end.


check_all_idle_or_unstarted(Pid, N) ->
    check_busy(Pid, N, 0).


check_all_busy(Pid, N) ->
    check_busy(Pid, N, N).


check_busy(Pid, N, NBusy) ->

    M = N - NBusy,

    P = fun(Unstarted, Idle, Busy) ->
                case length(Busy) of
                    NBusy ->
                        case length(Unstarted) + length(Idle) of
                            M -> ok;
                            Y -> {error, {"wrong number of unstarted/idle workers", Y}}
                        end;
                    Z -> {error, {"wrong number of busy workers", Z}}
                end
        end,

    Result = gen_pnet:state_property(Pid, P, ['Unstarted', 'Idle', 'Busy']),

    ?assertEqual(ok, Result).


check_all_busy_one_waiting(Pid, N) ->

    P = fun(Unstarted, Idle, Busy, Waiting) ->
                case length(Unstarted) + length(Idle) of
                    0 ->
                        case length(Busy) of
                            N ->
                                case length(Waiting) of
                                    1 -> ok;
                                    X -> {error, "wrong number of waiting workers", X}
                                end;
                            Y -> {error, {"wrong number of busy workers", Y}}
                        end;
                    Z -> {error, {"some unstarted/idle workers", Z}}
                end
        end,

    Result = gen_pnet:state_property(Pid,
                                     P,
                                     ['Unstarted', 'Idle', 'Busy', 'Waiting']),

    ?assertEqual(ok, Result).
