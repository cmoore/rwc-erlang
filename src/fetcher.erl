
-module( fetcher ).
-export( [ start/1, stop/0 ] ).

-include( "dorkinator.hrl" ).

start( Time ) ->
    register( fetcher, spawn( fun() ->
                                      fetch( Time ) end ) ).
stop() ->
    clock ! stop.

fetch( Time ) ->
    receive
        stop ->
            void
    after Time ->
%            fetch_all(),
            fetch( Time )
    end.


