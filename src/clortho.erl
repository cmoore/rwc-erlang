% Gozer, indeed.
-module( clortho ).
-export( [ start/0 ] ).

clortho_store( Key ) ->
    is_running(),
    rpc( { store, Key, Value } ).

clortho_lookup( Key ) ->
    is_running(),
    rpc( { lookup, Key } ).

clortho_loop() ->
    receive
        { From, { store, Key, Value } } ->
            put( Key, { ok, Value } ),
            From ! { clortho, true },
            keyd_loop();
        { From, { lookup, Key } } ->
            From ! { clortho, get( Key ) },
            clortho_loop()
    end.

is_running() ->
    case whereis( clortho ) of
        undefined ->
            Parent = self(),
            spawn( fun() ->
                           register( clortho, self() ),
                           Parent ! { registered, self() },
                           clortho_loop()
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

rpc( Q ) ->
    clortho ! { self(), Q },
    receive
        { clortho, Reply } ->
            Reply
    end.

start() ->
    is_running().

