
-module( u_handler ).
-export( [
          login_handler/1,
          logout_handler/1,
          register_handler/1
         ] ).
-include( "yaws_api.hrl" ).
-include( "rwc.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

logout_handler( A ) ->
    case rwc:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            users:update_auth( Vx#users.login, "" ),
            { redirect, "/u/login" }
    end.

login_handler( A ) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            Pf = pfactory:new( A ),
            { html, Pf:page( "login" ) };
        'POST' ->
            case rwc:validate( A, [
                                          "login",
                                          "password"
                                          ], fun validate_field/2 ) of
                { [ Login, Password ], [] } ->
                    case users:find_user( Login, Password ) of
                        [] ->
                            Pf = pfactory:new( A ),
                            { html, Pf:page( "login", [ { error, "Bad login/password." } ] ) };
                        Vx ->
                            [ Px | _ ] = Vx,
                            Pf = pfactory:new( A ),
                            AuthKey = binary_to_list(rwc:gen_key()),
                            users:update_auth( Px#users.login, AuthKey ),
                            [ { html, Pf:page( "qdirect" ) }, rwc:format_cookie( #session{ key = AuthKey } ) ]
                    end;
                _ ->
                    Pf = pfactory:new( A ),
                    { html, Pf:page( "login", [{error,"Bad login/password."}])}
            end
    end.

validate_field( "user_login", Login ) when length( Login ) > 1 ->
    ok;
validate_field( "user_login", _ ) ->
    short_value();
validate_field( "user_password", Password ) when length( Password ) > 1 ->
    ok;
validate_field( "user_password", _ ) ->
    short_value();
validate_field( "user_email", Email ) when length( Email ) > 4 ->
    ok;
validate_field( "user_email", _ ) ->
    short_value();
validate_field( "password", _ ) ->
    ok;
validate_field( "login", _ ) ->
    ok.

short_value() ->
    { error, "Value is too short!" }.

register_handler( A ) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            Pf = pfactory:new( A ),
            { html, Pf:page( "register" ) };
        'POST' ->
            case rwc:validate( A, [
                                   "user_login",
                                   "user_password",
                                   "user_email"
                                  ], fun validate_field/2 ) of
                { [ Login, Password, Email ], [] } ->
                    case users:find_user( Login ) of
                        [ { users, _, _, _, _, _, _ } ] ->
                            Pf = pfactory:new( A ),
                            { html, Pf:page( "register", [ { error, "You'll have to pick a different username." } ] ) };
                        [] ->
                            case users:write(
                                   #users { login = Login, password = rwc:hexdigest( Password ), email = Email, service_key = rwc:gen_key() }
                                  ) of
                                { atomic, ok } ->
                                    Pf = pfactory:new( A ),
                                    { html, Pf:page( "login", [ { error, "Successful!  (Start with the setup link at the top after you log in)" } ] ) };
                                _ ->
                                    Pf = pfactory:new( A ),
                                    { html, Pf:page( "register", [ { error, "Something screwed up.  Try again, please." } ] ) }
                            end
                    end
            end
    end.

