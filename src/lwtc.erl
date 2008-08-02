
%
% Light Weight Twitter Client
%
% I suck at naming things.  I'm much better at making them work.
%
%

%
% The idea here, at 2am, is that we grab everything from twitter
% and throw it into ETS and then make queries against that.
% I'm 90% sure i'll write a process that spawns off of this that
% will do automatic updates to the ETS data.
%
% We'll see how much of this actually works that way once I'm done.
%

-module( lwtc ).
-export( [
	  setup/1,
	  friends_timeline/0
	 ] ).

-author( "Clint Moore <hydo@mac.com>" ).
-version( "0.2" ).

setup( AuthInfo ) ->
    case AuthInfo of
	[ { login, Login, password, Password, mode, "twitter" } ] ->
	    kvs:store( Login, [ { login, Login }, { password, Password }, { service, twitter } ] );
	[ { login, Login, password, Password, mode, "identi.ca" } ] ->
	    kvs:store( Login, [ { password, Password }, { service, identica } ] );
	[ { login, Login, password, Password } ] -> % default to twitter.
	    kvs:store( Login, [ { password, Password }, { service, twitter } ] );
	_ ->
	    false
    end.

friends_timeline() ->
    case json_request( "http://twitter.com/statuses/friends_timeline.json" ) of
	{ error, _ } ->
	    error;
	[] ->
	    error;
	Data ->
	    Data
    end.

%
% Simplified url to json helpers.
%
json_request( Url ) ->
    case ets:lookup( lwtc, auth_info ) of
	[ { auth_info, { Login, Password } } ] ->
	    case http_auth_request( Url, Login, Password ) of
		{ ok, { _, _, Result } } ->
		    mochijson2:decode( Result );
		_ ->
		    { error, bad_result_from_http_request }
	    end;
	_ ->
	    { error, module_not_set_up }
    end.

http_auth_request( Url, User, Pass ) ->
    http:request( get, { Url, headers( User, Pass ) }, [], [] ).
headers( User, Pass ) ->
    UP = base64:encode( User ++ ":" ++ Pass ),
    Basic = lists:flatten( io_lib:fwrite( "Basic ~s", [ UP ] ) ),
    [ { "User-Agent", "Dorkpatrol/0.1" }, { "Authorization", Basic } ].

