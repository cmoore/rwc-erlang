
-module( t_handler ).
-export( [ out/1, reformat_twitter_data/1 ] ).
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
	    { html, Pf:page( "index" ) }
    end.


format_cookie( Sx ) ->
    Cookie = yaws_api:new_cookie_session( Sx ),
    yaws_api:setcookie( "dorkinator", Cookie, "/" ).
    
setup_handler( A, Pf ) ->
    case yaws_arg:method( A ) of
	'GET' ->
	    { html, Pf:page( "setup" ) };
	'POST' ->
	    AuthKey = gen_key(),
	    case dorkinator:validate( A, [
					  "twitter_login",
					  "twitter_password",
					  "identica_login",
					  "identica_password",
					  "ping_login",
					  "ping_password"
					 ], fun validate_field/2 ) of

		{ [ T_login, T_password, I_login, I_password, P_login, P_password ], [] } ->
		    kvs:store( AuthKey, [
					 { twitter_login, T_login },
					 { twitter_password, T_password },
					 { identica_login, I_login },
					 { identica_password, I_password },
					 { ping_login, P_login },
					 { ping_password, P_password }
					] ),
		    [ { html, Pf:page( "qdirect" ) }, format_cookie( #session{ key = AuthKey } ) ];
		_ ->
		    { html, Pf:page( "viewer", [ { error, "Something went horribly wrong." } ] ) }
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
			    { html, Px:page( "viewer", [ { twitter_messages, "1" }, { tmessages, reformat_twitter_data( Twitter_data ) } ] ) };
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
    case lists:keysearch( service, 1, Params ) of
	{ value, { service, twitter } } ->
	    case lists:keysearch( auth, 1, Params ) of
		{ value, { auth, AuthInfo } } ->
		    case lists:keysearch( twitter_login, 1, AuthInfo ) of
			{ value, { twitter_login, Tlogin } } ->
			    case lists:keysearch( twitter_password, 1, AuthInfo ) of
				{ value, { twitter_password, TPassword } } ->
				    case lwtc:setup( [ { login, Tlogin }, { password, TPassword }, { mode, twitter } ] ) of
					true ->
					    lwtc:friends_timeline( Tlogin );
					false ->
					    [ { error, "LWTC could not be set up." } ]
				    end;
				_ ->
				    [ { error, "Could not find the twitter password." } ]
			    end;
			_ ->
			    [ { error, "Could not find the twitter login." } ]
		    end;
		_ ->
		    [ { error, "pull_service_data: Could not find the passed auth info!?" } ]
	    end;
	_ ->
	    [ { error, "You need to specify a service type in setup." } ]
    end.

%
% The mochi json parser returns the keys in binary format
% so we've got to do some interpretive dance to get erlydtl to like it.
%

reformat_twitter_data( [] ) ->
    [];
reformat_twitter_data( [ Element | Rest ] ) ->
    case Element of
	{ struct, List } ->
	    case lists:keysearch( list_to_binary( "id" ), 1, List ) of
		{ value, { <<"id">>, Id } } ->
		    case lists:keysearch( list_to_binary( "text" ), 1, List ) of
			{ value, { <<"text">>, Text } } ->
			    lists:append( [[
					    { id, Id },
					    { text, Text },
					    { picture, element_from_user( List, <<"profile_image_url">> ) },
					    { name, element_from_user( List, <<"name">> ) }
					   ]], reformat_twitter_data( Rest ))
		    end
	    end
    end.

%
% This should probably be in lwtc - I'll refactor later, it's late and I want to see it work.
%

element_from_user( Message, Element ) ->
    case lists:keysearch( <<"user">>, 1, Message ) of
	{ value, { <<"user">>, { struct, UserList } } } ->
	    case lists:keysearch( Element, 1, UserList ) of
		{ value, { Element, User } } ->
		    binary_to_list( User )
	    end
    end.
