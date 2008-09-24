
-module( t_handler ).
-export( [ out/1, reformat_friends_data/1, merge_by_date/1, date_from_message/1, msg_date_to_local/1 ] ).
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
        "t/tweet" ->
            tweet_handler( A, Pf );
        _ ->
            { html, Pf:page( "index" ) }
    end.

tweet_handler( _A, Pf ) ->
    { html, Pf:page( "tweet" ) }.

format_cookie( Sx ) ->
    Cookie = yaws_api:new_cookie_session( Sx, (60 * 60 * 24 * 365) ),
    yaws_api:setcookie( "dorkinator", Cookie, "/" ).
    
setup_handler( A, Pf ) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            { html, Pf:page( "setup" ) };
        'POST' ->
            AuthKey = gen_key(),
            case dorkinator:validate( A, [
                                          "twitter_login",
                                          "twitter_password",
                                          "identica_login",
                                          "identica_password"
                                         ], fun validate_field/2 ) of

                { [ T_login, T_password, I_login, I_password ], [] } ->
                    kvs:store( AuthKey, [
                                         { twitter_login, T_login },
                                         { twitter_password, T_password },
                                         { identica_login, I_login },
                                         { identica_password, I_password }
                                        ] ),
                    [ { html, Pf:page( "qdirect" ) }, format_cookie( #session{ key = AuthKey } ) ];
                _ ->
                    { html, Pf:page( "viewer", [ { error, "Something went horribly wrong." } ] ) }
            end
    end.

validate_field( _X, _Y ) ->
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
                            Twitter_data = reformat_friends_data( friends_timeline( [ { service, twitter }, { auth, AuthInfo } ] ) ),
                            Identica_data = reformat_friends_data( friends_timeline( [ { service, identica }, { auth, AuthInfo } ] ) ),
                            { html, Px:page( "viewer", [
                                                        { twittermessages, Twitter_data },
                                                        { identicamessages, Identica_data }
                                                       ] ) };
                        _ ->
                            { redirect, "/t/setup" }
                    end;
                { error, no_session } ->
                    { redirect, "/t/setup" }
            end
    end.

friends_timeline( Info ) ->
    case Info of
        [ { service, Service }, { auth, AuthInfo } ] ->
            case pull_service_data(
                   field_from_auth( AuthInfo, list_to_atom(atom_to_list(Service) ++ "_login") ),
                   field_from_auth( AuthInfo, list_to_atom(atom_to_list(Service) ++ "_password") ),
                   Service, friends_timeline ) of
                { error, _ } ->
                    { error, pull_service_data_failed };
                Px ->
                    Px
            end
    end.

field_from_auth( Auth, Field ) ->
    case lists:keysearch( Field, 1, Auth ) of
        { value, { Field, Value } } ->
            Value;
        _ ->
            null
    end.

gen_key() ->
    Key = crypto:rand_bytes( 20 ),
    base64:encode( binary_to_list( Key ) ).

pull_service_data( Login, Password, Service, Request ) ->
    case lwtc:setup( [ { login, Login }, { password, Password }, { mode, Service } ] ) of
        true ->
            Px = lwtc:request( Login, Request ),
            case Px of
                { error, _ } ->
                    { error, lwtc_errored_out };
                { struct, Fv } ->
                    case lists:keysearch( list_to_binary( "error" ), 1, Fv ) of
                        { value, _ } ->
                            { error, lwtc_errored_out };
                        _ ->
                            Px
                    end;
                Ft ->
                    Ft
            end
    end.

%
% The mochi json parser returns the keys in binary format
% so we've got to do some interpretive dance to get erlydtl to like it.
%
reformat_friends_data( { error, pull_service_data_failed } ) ->
    [];
reformat_friends_data( [] ) ->
    [];
reformat_friends_data( [ Element | Rest ] ) ->
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
                                            { name, element_from_user( List, <<"name">> ) },
                                            { created, lists:keysearch( list_to_binary( "created_at" ), 1, List ) }
                                           ]], reformat_friends_data( Rest ))
                    end
            end
    end.

merge_by_date( [] ) ->
    [];
merge_by_date( [ Element | Rest ] ) ->
    { struct, Entry } = Element,
    case lists:keysearch( <<"created_at">>, 1, Entry ) of
        { value, { _, X } } ->
            io:format( "~p~n", [ binary_to_list( X ) ] );
        _ ->
            io:format( "Noes~n" )
    end,
    merge_by_date( Rest ).

element_from_user( Message, Element ) ->
    case lists:keysearch( <<"user">>, 1, Message ) of
        { value, { <<"user">>, { struct, UserList } } } ->
            case lists:keysearch( Element, 1, UserList ) of
                { value, { Element, User } } ->
                    binary_to_list( User )
            end
    end.

date_from_message( Message ) ->
    case Message of
        { struct, Entry } ->
            case lists:keysearch( <<"created_at">>, 1, Entry ) of
                { value, { _, X } } ->
                    X;
                _ ->
                    { error, date_not_found }
            end
    end.

msg_date_to_local( Date ) ->
    case string:tokens( binary_to_list( Date ), " " ) of
        [ _, Mon, Dom, Time, _, Year ] ->
            case string:tokens( Time, ":" ) of
                [ Hour, Minute, Seconds ] ->
                    { { Year, Mon, Dom }, { Hour, Minute, Seconds } };
                _ ->
                    { error, unparsable }
            end
    end.
