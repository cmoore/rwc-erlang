
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
-export( [ setup/2, update_friends_timeline/0 ] ).

-author( "Clint Moore <hydo@mac.com>" ).
-version( "0.1" ).

setup( User, Password ) ->
    ets:new( lwtc, [ set, named_table ] ),
    ets:insert( lwtc, { auth_info, { User, Password } } ).

update_friends_timeline() ->
    case grab_jsonified_request( "http://twitter.com/statuses/friends_timeline.json" ) of
	{ error, _ } ->
	    error;
	HugeFuckingList -> % and holy crap is it big...
	    [ write_out_friends_status( X ) || X <- HugeFuckingList ]
    end.

fl( K, L ) ->
    case lists:keysearch( list_to_binary( K ), 1, L ) of
	{ value, { _Key, Val } } ->
	    Val;
	_ ->
	    false
    end.

write_out_friends_status( Element ) ->
    case Element of
	{ status, List } ->
	    io:format( "Screen Name: ~p~n", [ lists:keysearch(list_to_binary( "screen_name" ), 2, List ) ] );
	%    ets:insert( lwtc, { fl("id",List),
	%			{ name, fl("name",List) },
	%			{ screen_name, fl("screen_name",List) },
	%			{ text, fl("text",List) },
	%			{ reply_to, fl("in_reply_to_status_id",List) } } );
	_ ->
	    false
    end.

%
% Simplified url to json helpers.
%
grab_jsonified_request( Url ) ->
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
