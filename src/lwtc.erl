%
% @author Clint Moore <hydo@mac.com>
% @copyright 2008 Clint Moore
% @doc
% A very lightweight module to communicate with twitter and Laconica compatible
% microblogging services.
% @end
%
% The MIT License
%
% Copyright (c) 2008 Roberto Saccon, Evan Miller
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
%
%
%

-module( lwtc ).
-export( [ setup/1, request/2 ] ).

-author( "Clint Moore <hydo@mac.com>" ).
-version( "0.2" ).

% @spec ( [ 
%          { login, "login_name" },
%          { password, "password" },
%          { mode, twitter|identica } 
%         ] ) -> true | false
% @doc sets up the authentication information for future requests.
% @end
setup( AuthInfo ) ->
    case AuthInfo of
	[ { login, Login }, { password, Password }, { mode, twitter } ] ->
	    kvs:store( Login, [ { login, Login }, { password, Password }, { service, twitter } ] );
	[ { login, Login }, { password, Password }, { mode, identica } ] ->
	    kvs:store( Login, [ { password, Password }, { service, identica } ] );
	[ { login, Login }, { password, Password } ] -> % default to twitter.
	    kvs:store( Login, [ { password, Password }, { service, twitter } ] );
	_ ->
	    false
    end.

% @spec ( "login_name", request ) -> List
% @doc
% performs the requested request.(heh)
% currently supported request atoms are
% friends_timeline, user_timeline, public_timeline, and replies
% support for more coming soon.
% @end
request( Login, Request ) ->
    case kvs:lookup( Login ) of
	{ ok, List } ->
	    case lists:keysearch( service, 1, List ) of
		{ value, { service, twitter } } ->
		    Url = head_for_service( twitter ) ++ url_for_action( Request ),
		    case json_request( Login, Url ) of
			{ error, _ } ->
			    error;
			[] ->
			    error;
			Data ->
			    Data
		    end;
		_ ->
		    { error, no_such_service }
	    end
    end.

%
% End of user-serviceable parts.
%

json_request( Login, Url ) ->
    case kvs:lookup( Login ) of
	{ ok, [ { login, Login }, { password, Password }, { _, _ } ] } ->
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

url_for_action( Action ) ->
    case Action of
	friends_timeline ->
	    "friends_timeline.json";
	user_timeline ->
	    "user_timeline.json";
	public_timeline ->
	    "public_timeline.json";
	replies ->
	    "replies.json";
	 _ ->
	    { error, no_such_action }
    end.

head_for_service( Service ) ->
    case Service of
	twitter ->
	    "http://www.twitter.com/statuses/";
	identica ->
	    "http://identi.ca/api/statuses/";
	_ ->
	    { error, no_such_service }
    end.
