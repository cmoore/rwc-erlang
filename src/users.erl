
-module( users ).
-export( [
          e/1,
          find_user/1,
          find_user/2,
          add_user/2,
          delete_user/1,
          service_key/1,
          update_auth/2,
          auth_confirm/1,
          enable_user/1,
          disable_user/1,
          write/1,
          users/0
         ] ).
-include_lib( "stdlib/include/qlc.hrl" ).
-include( "dorkinator.hrl" ).

e( Query ) ->
    { atomic, Val } = mnesia:transaction( fun() ->
                                                  qlc:e( Query ) end ),
    Val.

%
% Find a user
% (identify user)
find_user( Username ) ->
    find_user( Username, none ).
find_user( Login, Password ) ->
    case Password of
        none ->
            e( qlc:q( [ X || X <- mnesia:table(users), X#users.login =:= Login ] ) );
        _ ->
            e(
              qlc:q(
                [ X || X <- mnesia:table( users ),
                       X#users.login =:= Login,
                       X#users.password =:= dorkinator:hexdigest( Password ) ]
               ) )
    end.
    
%
% Make a new user.
%(select user)
add_user( Login, Password ) ->
    add_user( Login, Password, "" ).
add_user( Login, Password, Auth ) ->
    Px = dorkinator:hexdigest( Password ),
    User = #users{
      login = Login,
      password = Px,
      service_key = dorkinator:gen_key(),
      auth = Auth
     },
    mnesia:transaction( fun() ->
                                mnesia:write( User )
                        end ).

delete_user( Login ) ->
    Px = find_user( Login ),
    mnesia:transaction( fun() ->
                                mnesia:delete( { users, Px } )
                        end ).

auth_confirm( Auth ) ->
   case e( qlc:q( [ X || X <- mnesia:table(users),
                         X#users.auth =:= Auth ]
                 ) ) of
       [] ->
           false;
       Px ->
           Px
   end.

service_key( Login ) ->
    [ Px | _ ] = find_user( Login ),
    Px#users.service_key.

update_auth( Login, Auth ) ->
    case users:find_user( Login ) of
        [] ->
            false;
        Vx ->
            [ Px | _ ] = Vx,
            Fv = Px#users{ auth = Auth },
            mnesia:transaction( fun() ->
                                        mnesia:write( Fv )
                                end )
    end.

enable_user( Login ) ->
    [ Px | _ ] = users:find_user( Login ),
    Rv = Px#users{ active = yes },
    mnesia:transaction( fun() ->
                                mnesia:write( Rv )
                        end ).

disable_user( Login ) ->
    [ Px | _ ] = users:find_user( Login ),
    Rv = Px#users{ active = no },
    mnesia:transaction( fun() ->
                                mnesia:write( Rv )
                        end ).
write( Px ) ->
    mnesia:transaction( fun() ->
                                mnesia:write( Px )
                        end ).

users() ->
    users:e( qlc:q( [ X || X<-mnesia:table( users ) ] ) ).
