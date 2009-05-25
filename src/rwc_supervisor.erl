
-module( rwc_supervisor ).
-behaviour( supervisor ).

-export( [ start/0, start_link/1, init/1 ] ).

start() ->
    spawn( fun() ->
                   supervisor:start_link( { local, ?MODULE }, ?MODULE, _Arg = [] )
           end ).

start_link( Arg ) ->
    supervisor:start_link( { local, ?MODULE }, ?MODULE, Arg ).

init( [] ) ->
    gen_event:swap_handler( alarm_handler, { alarm_handler, swap }, { my_alarm_handler, xyz } ),
    { ok, { { one_for_one, 3, 10 },
            [ { tag1, { rwc, start, [] },
                permanent, 10000, worker, [ rwc ] },
              { tag2, { couch_server, start, [ "etc/local.ini" ] },
                permanent, 10000, worker, [ couch_server ] } ] } }.


