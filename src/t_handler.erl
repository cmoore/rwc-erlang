
-module( t_handler ).
-export( [ out/1 ] ).
-include( "yaws_api.hrl" ).
-include( "dorkinator.hrl" ).

out( Pf ) ->
    A = Pf:server_args(),
    Path = A#arg.appmoddata,
    case Path of
	"t/setup" ->
	    setup_handler( A, Pf );
	"t/viewer" ->
	    viewer_handler( A, Pf );
	_ ->
	    { html, Pf:page( "index.html" ) }
    end.


format_cookie( Sx ) ->
    Cookie = yaws_api:new_cookie_session( Sx ),
    yaws_api:setcookie( "dorkinator", Cookie, "/" ).
    
setup_handler( A, Pf ) ->
    case yaws_arg:method( A ) of
	'GET' ->
	    { html, Pf:page( "setup.html" ) };
	'POST' ->
	    AuthKey = gen_key(),
	    case dorkinator:validate( A, [ "twitter_login", "twitter_password", "identica_login", "identica_password", "ping_login", "ping_password" ], fun validate_field/2 ) of
		{ [ T_login, T_password, I_login, I_password, P_login, P_password ], [] } ->
		    kvs:store( AuthKey, [
					 { twitter_login, T_login },
					 { twitter_password, T_password },
					 { identica_login, I_login },
					 { identica_password, I_password },
					 { ping_login, P_login },
					 { ping_password, P_password }
					] ),
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( #session{ key = AuthKey } ) ];
		_ ->
		    { html, Pf:page( "viewer.html", [ { error, "Something went horribly wrong." } ] ) }
	    end
    end.
			
validate_field( X, Y ) ->
    io:format( "X: ~p Y: ~p~n", [ X, Y ] ),
    ok.

viewer_handler( A, Px ) ->
    H = A#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val( "dorkinator", C ) of
	[] ->
	    { redirect, "/t/setup" };
	Cookie ->
	    case yaws_api:cookieval_to_opaque( Cookie ) of
		{ ok, Val } ->
		    Key = Val#session.key,
		    case kvs:lookup( Key ) of
			{ ok, AuthInfo } ->
			    % We have the login and password that was stored from setup!
			    Twitter_data = pull_service_data( [ { service, twitter }, { auth, AuthInfo } ] ),
			    Identica_data = pull_service_data( [ { service, identica }, { auth, AuthInfo } ] ),
			    { html, Px:page( "viewer.html", [
							     { twitter_messages, true },
							     { twitter, Twitter_data }
							     ] ) };
			_ ->
			    { redirect, "/t/setup" }
		    end;
		{ error, no_session } ->
		    { redirect, "/t/setup" }
	    end
    end.

gen_key() ->
    Key = crypto:rand_bytes( 20 ),
    base64:encode( binary_to_list( Key ) ).

pull_service_data( Params ) ->
    case lists:keysearch( "service", 1, Params ) of
	{ ok, twitter } ->
	    % Pull and reparse twitter data.
	    [ messages, [ { id, "21212" }, { text, "HOnk!" } ] ];
	{ ok, identica } ->
	    [ [ { status, { id, "1212" }, { text, "Hello from identica" } } ] ];
	_ ->
	    [ [ { status, { id, "8675309" }, { text, "No setup for this service" } } ] ]
    end.
