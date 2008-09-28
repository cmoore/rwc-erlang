
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
        "u/login" ->
            login_handler( Args, Pf );
        _ ->
            login_handler( Args, Pf )
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
                        Px ->
                            % login was successful
                            io:format( "Login ok.~n" ),
                            AuthKey = dorkinator:gen_key(),
                            users:update_auth( Px#users.login, AuthKey ),
                            [ { html, Pf:page( "qdirect" ) }, dorkinator:format_cookie( #session{ key = AuthKey } ) ]
                    end
            end
    end.

validate_field( _X, _Y ) ->
    ok.
