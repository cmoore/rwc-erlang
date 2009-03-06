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
% Copyright (c) 2008 Clint Moore
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
% @drivel
% 
% I'd like for this to be completely standalone but alas, I have much more faith
% in Bob Ippolito's ability to write a json parser than my own.  And it's for
% this reason that mochijson2.erl is a required dependency.
%
% @end
%

-module( lwtc ).
-export( [
          setup/1, request/3, request/2,
          update/2, update_location/2,
          near_me/3, near_me/4, nrequest/4
         ] ).
-author( "Clint Moore <hydo@mac.com>" ).
-version( "0.5" ).
-include( "dorkinator.hrl" ).
-license( { mit_license, "http://scutil.com/license.tmpl" } ).

% @spec ( [ 
%          { login, "login_name" },
%          { password, "password" },
%          { mode, twitter|identica } 
%         ] ) -> true | false
% @doc sets up the authentication information for future requests.
% @end

setup( [ { login, Login }, { password, Password }, { service, identica } ] ) ->
    f_setup( Login, Password, (Login ++ "-identica") );
setup( [ { login, Login }, { password, Password }, _ ] ) ->
    f_setup( Login, Password, (Login ++ "-twitter" ) ).
f_setup( Lg, Pw, Sv ) ->
    keyd_store( Sv, [ { login, Lg }, { password, Pw } ] ),
    { ok, Sv }.


update_location( Info, Latlon ) ->
    Location = "location=" ++ yaws_api:url_encode( Latlon ),
    http:request( post,
                  { url_for_action( update_location, Info#services.service ),
                    headers( Info#services.username, Info#services.password ),
                    "application/x-www-form-urlencoded",
                    Location }, [], [] ).

update( Info, Message ) ->
    http:request( post, {
                    url_for_action( update, Info#services.service ),
                    headers( Info#services.username, Info#services.password ),
                    "application/x-www-form-urlencoded",
                    ( "source=" ++ yaws_api:url_encode( "royalewithcheese" ) ++ "&status=" ++ yaws_api:url_encode( Message ) )
                   }, [], [] ).


near_me( Login, Password, Location ) ->
    near_me( Login, Password, Location, 50 ).

near_me( Login, Password, Location, Length ) ->
    Message = Location ++ "," ++ Length ++ "km",
    Yoorl = "http://search.twitter.com/search.json?rpp=100&geocode="
        ++ yaws_api:url_encode( Message ),
    json_request( get, Login, Password, Yoorl ).


nrequest( _Login, _Password, Service, Request ) when Service == "identica",
                                                     Request == direct_messages ->
    [];
nrequest( Login, Password, Service, Request ) ->
    json_request( get, Login, Password, url_for_action( Request, Service ) ).


request( Identifier, Request ) ->
    request( Identifier, Request, "" ).
request( Identifier, Request, _Args ) ->
    case auth_from_id( Identifier ) of
        [ { login, Login }, { password, Password }, { service, Service } ] ->
            json_request( get, Login, Password, url_for_action( Request, Service ) );
        _ ->
            false
    end.


auth_from_id( Id ) ->
    case keyd_lookup( Id ) of
        { ok, Info } ->
            Info;
        _ ->
            false
    end.


json_request( post, Login, Password, Url ) ->
    jsf( http:request( post, { Url, headers( Login, Password ) }, [], [] ) );
json_request( get, Login, Password, Url ) ->
    jsf( http:request( get, { Url, headers( Login, Password ) }, [], [] ) ).


jsf( Result ) ->
    case Result of
        { ok, { _, _, Res } } ->
            case Res of
                "Could not authenticate you." ->
                    "";
                _ ->
                    mochijson2:decode( Res )
            end;
        { error, Reason } ->
            { error, Reason }
    end.


headers( User, Pass ) ->
    UP = base64:encode( User ++ ":" ++ Pass ),
    Basic = lists:flatten( io_lib:fwrite( "Basic ~s", [ UP ] ) ),
    [ { "User-Agent", "RWC/0.45" }, { "Authorization", Basic } ].


url_for_action( Action, _Service ) when Action == trends ->
    "http://search.twitter.com/statuses/trends.json";
url_for_action( Action, Service ) ->
    head_for_service( Service ) ++ action_path( Action ).

action_path( direct_messages ) -> "direct_messages.json";
action_path( update ) -> "statuses/update.json";
action_path( friends_timeline ) -> "statuses/friends_timeline.json?count=100&";
action_path( user_timeline ) -> "statuses/user_timeline.json";
action_path( public_timeline ) -> "statuses/public_timeline.json";
action_path( update_location ) -> "account/update_profile.json";
action_path( replies ) -> "statuses/replies.json".


head_for_service( Service ) when Service == "twitter" ->
    "http://www.twitter.com/";
head_for_service( Service ) when Service == "identica" ->
    "http://identi.ca/api/".

%
% Thank you, erlang-questions for your nifty post archives
%

is_running() ->
    case whereis( keyd ) of
        undefined ->
            Parent = self(),
            spawn( fun() ->
                           register( keyd, self() ),
                           Parent ! { registered, self() },
                           keyd_loop()
                   end ),
            receive
                { registered, P } ->
                    P
            after 5000 ->
                    is_running()
            end;
        P ->
            P
    end.

%
% From Joe Armstrong's book
%
keyd_store( Key, Value ) ->
    is_running(),
    rpc( { store, Key, Value } ).

keyd_lookup( Key ) ->
    is_running(),
    rpc( { lookup, Key } ).

rpc( Q ) ->
    keyd ! { self(), Q },
    receive
        { keyd, Reply } ->
            Reply
    end.

keyd_loop() ->
    receive
        { From, { store, Key, Value } } ->
            put( Key, { ok, Value } ),
            From ! { keyd, true },
            keyd_loop();
        { From, { lookup, Key } } ->
            From ! { keyd, get( Key ) },
            keyd_loop()
    end.

