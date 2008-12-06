
-module( u_handler ).
-export( [
          out/1
         ] ).
-include( "yaws_api.hrl" ).
-include( "dorkinator.hrl" ).

out( Pf ) ->
    Args = Pf:server_args(),
    Path = Args#arg.appmoddata,
    case Path of
        "u/logout" ->
            logout_handler( Args, Pf );
        "u/register" ->
            register_handler( Args, Pf );
        "u/login" ->
            login_handler( Args, Pf );
        _ ->
            case dorkinator:auth_info( Args ) of
                false ->
                    login_handler( Args, Pf );
                _ ->
                    { redirect, "/t/viewer" }
            end
    end.

logout_handler( A, _Pf ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            users:update_auth( Vx#users.login, "" ),
            { redirect, "/u/login" }
    end.

login_handler( A, Pf ) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            { html, Pf:page( "login" ) };
        'POST' ->
            case dorkinator:validate( A, [
                                          "login",
                                          "password"
                                          ], fun validate_field/2 ) of
                { [ Login, Password ], [] } ->
                    case users:find_user( Login, Password ) of
                        [] ->
                            { html, Pf:page( "login", [ { error, "Bad login/password." } ] ) };
                        Vx ->
                            [ Px | _ ] = Vx,
                            AuthKey = binary_to_list(dorkinator:gen_key()),
                            users:update_auth( Px#users.login, AuthKey ),
                            [ { html, Pf:page( "qdirect" ) }, dorkinator:format_cookie( #session{ key = AuthKey } ) ]
                    end;
                _ ->
                    { html, Pf:page( "login", [{error,"Bad login/password."}])}
            end
    end.

short_value() ->
    { error, "Value is too short!" }.

validate_field( "login", Login ) when length( Login ) > 1 ->
    ok;
validate_field( "login", _ ) ->
    short_value();
validate_field( "password", Password ) when length( Password ) > 4 ->
    ok;
validate_field( "password", _ ) ->
    short_value();
validate_field( "user_email", Email ) when length( Email ) > 4 ->
    ok;
validate_field( "user_email", _ ) ->
    short_value().

register_handler( A, Pf ) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            { html, Pf:page( "register" ) };
        'POST' ->
            case dorkinator:validate( A, [
                                          "user_login",
                                          "user_password",
                                          "user_email"
                                         ], fun validate_field/2 ) of
                { [ Login, Password, Email ], [] } ->
                    case users:find_user( Login ) of
                        [ { users, _, _, _, _, _, _ } ] ->
                            { html, Pf:page( "register", [ { error, "You'll have to pick a different username." } ] ) };
                        [] ->
                            case users:write(
                                   #users { login = Login, password = dorkinator:hexdigest( Password ), email = Email, service_key = dorkinator:gen_key() }
                                  ) of
                                { atomic, ok } ->
                                    { html, Pf:page( "login", [ { error, "Successful!  (Start with the setup link at the top after you log in)" } ] ) };
                                _ ->
                                    { html, Pf:page( "register", [ { error, "Something screwed up.  Try again, please." } ] ) }
                            end
                    end
            end
    end.
