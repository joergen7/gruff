-module( gruff_wrk ).

%%====================================================================
%% Callback definitions
%%====================================================================

-callback start_link( WrkArgs ) -> {ok, Pid}
                                 | {error, {already_started, Pid}}
                                 | {error, Reason}
when WrkArgs :: proplists:proplist(),
     Pid     :: pid(),
     Reason  :: _.