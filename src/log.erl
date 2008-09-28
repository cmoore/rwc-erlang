
-module(log).
-export( [ f/1 ] ).

f( Message ) ->
    io:format( "LOG: ~p~n", [ Message ] ).
