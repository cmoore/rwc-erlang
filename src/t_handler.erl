
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
	    case dorkinator:validate( A, [ "twitter_login", "twitter_password", "ping_login", "ping_password" ], fun validate_field/2 ) of
		{ [ Tlogin, Tpassword, Plogin, Ppassword ], _ } ->
		    Session = #session{ tlogin = Tlogin, tpassword = Tpassword, plogin = Plogin, ppassword = Ppassword },
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( Session ) ];
		{ [ Tlogin, Tpassword, [], [] ], _ } ->
		    Ss = #session{ tlogin = Tlogin, tpassword = Tpassword },
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( Ss ) ];
		{ [ [], [], Plogin, Ppassword ], _ } ->
		    [ { html, Pf:page( "qdirect.html" ) }, format_cookie( #session{ plogin = Plogin, ppassword = Ppassword } ) ];
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
	    { html, Px:page( "viewer.html", [ { error, "Can't find any auth info.  Click setup above." } ] ) };
	Cookie ->
	    { ok, Session } = yaws_api:cookieval_to_opaque( Cookie ),
	    TwitterData = case pull_twitter_data( Session ) of
			      { ok, Bundle } ->
				  [ { twitter, true }, { twitter_data, Bundle } ];
			      { error, _ } ->
				  [ { twitter, false } ]
			  end,
	    IdenticaData = case pull_identica_data( Session ) of
			       { ok, Bundle } ->
				   [ { identica, true }, { identica_data, Bundle } ];
			       { error, _ } ->
				   [ { identica, false } ]
			   end,
	    { html, Px:page( "viewer.html", lists:merge( TwitterData, IdenticaData ) ) }
    end.

pull_twitter_data( Session ) ->
    case proplists:lookup( tlogin, Session ) of
	{ _, Login } ->
	    case proplists:lookup( tpassword, Session ) of
		{ _, Password } ->
		    lwtc:setup( Login, Password ),
		    case lwtc:update_friends_timeline() of
			error ->
			    { error, something_messed_up };
			{ ok, Bundle } ->
			    { ok, reformat_twitter( Bundle ) }
		    end;
		_ ->
		    { error, something }  % TODO - these could use some better explanations.
	    end;
	_ ->
	    { error, something }
    end.
