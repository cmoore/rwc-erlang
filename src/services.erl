
-module( services ).
-export( [ e/1 ] ).
-include_lib( "stdlib/include/qlc.hrl" ).

e( Query ) ->
    { atomic, Val } = mnesia:transaction( fun() ->
                                                  qlc:e( Query ) end ),
    Val.
