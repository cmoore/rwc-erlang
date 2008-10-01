
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
            login_handler( Args, Pf )
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

validate_field( _X, _Y ) ->
    ok.

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
                    case Email of  % Yes, this is what validate_fields/2 is for but it's really late and I want this to work.
                        [] ->      % I WILL FIX THE STUPID TOMORROW.
                            { html, Pf:page( "register", [ { error, "You need to provide an email address." } ] ) };
                        _ ->
                            case Password of
                                [] ->
                                    { html, Pf:page( "register", [ { error, "No password?" } ] ) };
                                _ ->
                                    case Login of
                                        [] ->
                                            { html, Pf:page( "register", [ { error, "No login?" } ] ) };
                                        _ ->
                                            case users:find_user( Login ) of
                                                [ { users, _, _, _, _, _, _ } ] ->
                                                    { html, Pf:page( "register", [ { error, "You'll have to pick a different username." } ] ) };
                                                [] ->
                                                    Px = #users{
                                                      login = Login,
                                                      password = dorkinator:hexdigest( Password ),
                                                      email = Email,
                                                      service_key = dorkinator:gen_key()
                                                     },
                                                    case users:write( Px ) of
                                                        { atomic, ok } ->
                                                            { html, Pf:page( "login", [ { error, "Successful!  Have fun.<br/><br/>(Start with the setup link at the top after you log in)" } ] ) };
                                                        _ ->
                                                            { html, Pf:page( "register", [ { error, "Something screwed up.  Try again, please." } ] ) }
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.
