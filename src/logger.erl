
-module( logger ).
-export( [ log/1 ] ).


is_running() ->
    case whereis( logger ) of
        undefined ->
            Parent = self(),
            spawn( fun() ->
                           register( logger, self() ),
                           Parent ! { registered, self() },
                           logger_loop()
                   end ),
            receive
                { registered, P } ->
                    P
            after 5000 ->
                    is_running()
            end;
        P ->
            P
    end.

log( Message ) ->
    is_running(),
    { ok, S } = file:open( "dorkinator.txt", [append] ),
    io:format( S, "~s~n", [ Message ] ),
    file:close( S ).


logger_loop() ->
    receive
        { From, { log, Message } } ->
            log( Message ),
            From ! { log, true },
            logger_loop()
    end.
