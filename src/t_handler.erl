
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
	    case dorkinator:validate( A, [ "twitter_login", "twitter_password", "ping_login", "ping_password" ], fun validate_field/2 ) of
		{ [ Tlogin, Tpassword, Plogin, Ppassword ], _ } ->
		    Session = #session{ key = AuthKey },
		    kvs:store( AuthKey, [ { twitter_login, Tlogin }, { twitter_password, Tpassword }, { ping_login, Plogin }, { ping_password, Ppassword } ] ),
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( Session ) ];
		{ [ Tlogin, Tpassword, [], [] ], _ } ->
		    Ss = #session{ key = AuthKey},
		    kvs:store( AuthKey, [ { twitter_login, Tlogin }, { twitter_password ,Tpassword } ] ),
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( Ss ) ];
		{ [ [], [], Plogin, Ppassword ], _ } ->
		    kvs:store( AuthKey, [ { ping_login, Plogin }, { ping_password, Ppassword } ] ),
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( #session{ key = AuthKey } ) ];
		_ ->
		    { html, Pf:page( "setup.html",
				     [ { error, "Something went wrong with your input." } ] ) }
	    end;
	_ ->
	    { html, Pf:page( "setup.html", [ { error, "Your input didn't validate for whatever reason." } ] ) }
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
			{ ok, _AuthInfo } ->
			    % We have the login and password that was stored from setup!
			    { html, Px:page( "viewer.html", [ { error, "Found your login information." } ] ) };
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
