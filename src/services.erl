
-module( services ).
-export( [ e/1, add_service/4, for_user/1, delete/1, by_user/1, cred_for_service/2, all/0 ] ).

-include_lib( "stdlib/include/qlc.hrl" ).
-include( "rwc.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

e( Query ) ->
    { atomic, Val } = mnesia:transaction( fun() ->
                                                  qlc:e( Query ) end ),
    Val.

add_service( OurUserName, Login, Password, Type ) ->
    Skey = users:service_key( OurUserName ),
    Svc = #services{
      username = Login,
      password = Password,
      service = Type,
      service_key = Skey,
      idx = binary_to_list(rwc:gen_key())
     },
    mnesia:transaction( fun() ->
                                mnesia:write( Svc )
                        end ).

for_user( Login ) ->
    format( by_user( Login ) ).

format( [] ) ->
    [];
format( [ Px | Rst ] ) ->
    lists:append( [[
                   { service, Px#services.service },
                   { login, Px#services.username }
                   ]], format( Rst ) ).

by_user( Login ) ->
    e(
      qlc:q(
        [ X || X <- mnesia:table( services ),
               X#services.service_key =:= users:service_key( Login )]
        )).

delete( Idx ) ->
    mnesia:transaction( fun() ->
                                mnesia:delete( { services, Idx } )
                        end ).

cred_for_service( Login, Service ) ->
    [ Us | _ ] = users:find_user( Login ),
    [ Px | _ ] = e(
                   qlc:q(
                     [ X || X <- mnesia:table( services ),
                            X#services.service_key =:= Us#users.service_key,
                            X#services.service =:= Service ] ) ),
    Px.

all() ->
    e(
      qlc:q(
        [ X || X <- mnesia:table( services ) ] ) ).
