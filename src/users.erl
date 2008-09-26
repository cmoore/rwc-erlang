
-module( users ).
-export( [
          e/1,
          find_user/1,
          find_user/2,
          add_user/2,
          update_user/3,
          update_user/2,
          delete_user/1,
          auth_confirm/1
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
            e(
              qlc:q(
                [ X || X <- mnesia:table(users),
                       X#users.login =:= Login ]
               ) );
        _ ->
            e(
              qlc:q(
                [ X || X <- mnesia:table( users ),
                       X#users.login =:= Login,
                       X#users.password =:= Password ]
                ) )
    end.
    
%
% Make a new user.
%(select user)
add_user( Login, Password ) ->
    add_user( Login, Password, "" ).
add_user( Login, Password, Auth ) ->
    User = #users{
      login = Login,
      password = Password,
      service_key = Auth
     },
    mnesia:transaction( fun() ->
                                mnesia:write( User )
                        end ).

% (update user)
update_user( Login, Password ) ->
    update_user( Login, Password, no ).
update_user( Login, Password, Auth ) ->
    case find_user( Login ) of
        [ #users{ login = _, password = _, service_key = ServiceKey, auth = OldAuth } ] ->
            Px = #users{
              login = Login,
              password = Password,
              service_key = ServiceKey },
            Pxx = case Auth of
                      no ->
                          Px#users{auth = OldAuth};
                      _ ->
                          Px#users{auth = Auth}
                  end,
            mnesia:transaction( fun() ->
                                        mnesia:write( Pxx )
                                end )
    end.

delete_user( Login ) ->
    case find_user( Login ) of
        [ { users, Login, _, _, _ } ] ->
            Rx = #users{ login = Login },
            mnesia:transaction( fun() ->
                                        mnesia:delete( Rx )
                                end );
        _ ->
            { error, no_such_user }
    end.

auth_confirm( Auth ) ->
   case e( qlc:q( [ X || X <- mnesia:table(users),
                         X#users.auth =:= Auth ]
                 ) ) of
       [] ->
           false;
       Px ->
           Px
   end.
