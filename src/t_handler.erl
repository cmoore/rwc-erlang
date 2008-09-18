
-module( t_handler ).
-export( [ out/1, reformat_friends_data/1 ] ).
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
                            Twitter_data = get_friends_timeline( [ { service, twitter }, { auth, AuthInfo } ] ),
                            Identica_data = get_friends_timeline( [ { service, identica }, { auth, AuthInfo } ] ),
                            { html, Px:page( "viewer", [
                                                        { twittermessages, reformat_friends_data( Twitter_data ) },
                                                        { identicamessages, reformat_friends_data( Identica_data ) }
                                                       ] ) };
                        _ ->
                            { redirect, "/t/setup" }
                    end;
                { error, no_session } ->
                    { redirect, "/t/setup" }
            end
    end.

get_friends_timeline( Info ) ->
    case Info of
        [ { service, Service }, { auth, AuthInfo } ] ->
            pull_service_data(
              field_from_auth( AuthInfo, list_to_atom(atom_to_list(Service) ++ "_" ++ atom_to_list(login)) ),
              field_from_auth( AuthInfo, list_to_atom(atom_to_list(Service) ++ "_" ++ atom_to_list(password)) ),
              Service, friends_timeline )
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
            lwtc:request( Login, Request );
        { error, Reason } ->
            Reason
    end.

%
% The mochi json parser returns the keys in binary format
% so we've got to do some interpretive dance to get erlydtl to like it.
%

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

element_from_user( Message, Element ) ->
    case lists:keysearch( <<"user">>, 1, Message ) of
        { value, { <<"user">>, { struct, UserList } } } ->
            case lists:keysearch( Element, 1, UserList ) of
                { value, { Element, User } } ->
                    binary_to_list( User )
            end
    end.

