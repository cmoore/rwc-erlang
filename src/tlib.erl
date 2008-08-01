
-module( tlib ).
-export( [ test/0, http_auth_request/3 ] ).

http_auth_request( Url, User, Pass ) ->
    http:request( get, { Url, headers( User, Pass ) }, [], [] ).

test() ->
    http_auth_request( "http://www.twitter.com/statuses/friends_timeline.json", "hydo", "h4r01d" ).

headers( nil, nil ) ->
    [ { "User-Agent", "Dorkpatrol/0.1" } ];
headers( User, Pass ) ->
    UP = base64:encode( User ++ ":" ++ Pass ),
    Basic = lists:flatten( io_lib:fwrite( "Basic ~s", [ UP ] ) ),
    [ { "User-Agent", "Dorkpatrol/0.1" }, { "Authorization", Basic } ].
