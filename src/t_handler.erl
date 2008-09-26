
-module( t_handler ).
-export( [ out/1,
           tfm/1,
           reformat_friends_data/2,
           sort_messages/1  ] ).
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
        "t/post" ->
            tweet_handler( A, Pf );
        _ ->
            { html, Pf:page( "index" ) }
    end.

tweet_handler( A, _ ) ->
    case( A#arg.req)#http_request.method of
        'GET' ->
            { redirect, "/t/viewer" };
        'POST' ->
            case dorkinator:validate( A, [
                                          "post_identica",
                                          "post_twitter",
                                          "message"
                                          ], fun validate_field/2 ) of
                { [ _Ipost, Tpost, Message ], [] } ->
                    Info = auth_info( A ),
                    case Tpost of
                        "on" ->
                            case lists:keysearch( twitter_login, 1, Info ) of
                                { _, { twitter_login, Tlogin } } ->
                                    case lists:keysearch( twitter_password, 1, Info ) of
                                        { _, { twitter_password, TPass } } ->
                                            case lwtc:setup( [ { login, Tlogin }, { password, TPass }, { mode, twitter } ] ) of
                                                { ok, Id } ->
                                                    io:format( "Using Id: ~p~n", [ Id ] ),
                                                    lwtc:update( Id, update, Message )
                                            end
                                    end
                            end
                    end
            end,
            { redirect, "/t/viewer" }
    end.

% TODO - The cookie needs to never expire and this doesn't work.
format_cookie( Sx ) ->
    Cookie = yaws_api:new_cookie_session( Sx ),
    yaws_api:setcookie( "dorkinator", Cookie, "/", "'Wed 01-01-2020 00:00:00 GMT'" ).
    
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
                            Twitter_data = reformat_friends_data( friends_timeline( [ { service, twitter }, { auth, AuthInfo } ] ), twitter ),
                            Identica_data = reformat_friends_data( friends_timeline( [ { service, identica }, { auth, AuthInfo } ] ), identica ),
                            All_messages = sort_messages( lists:append( Twitter_data, Identica_data ) ),
                            { html, Px:page( "viewer", [
                                                        { twittermessages, lists:reverse( All_messages ) }
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
        { ok, Id } ->
            Px = lwtc:request( Id, Request ),
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
reformat_friends_data( { error, pull_service_data_failed }, _ ) ->
    [];
reformat_friends_data( [], _ ) ->
    [];
reformat_friends_data( [ Element | Rest ], Service ) ->
    case Element of
        { struct, List } ->
            case lists:keysearch( list_to_binary( "id" ), 1, List ) of
                { value, { <<"id">>, Id } } ->
                    case lists:keysearch( list_to_binary( "text" ), 1, List ) of
                        { value, { <<"text">>, Text } } ->
                            { value, { <<"created_at">>, Date } } = lists:keysearch( list_to_binary( "created_at" ), 1, List ),
                            lists:append( [[
                                            { svc, atom_to_list(Service) },
                                            { id, Id },
                                            { text, Text },
                                            { picture, element_from_user( List, <<"profile_image_url">> ) },
                                            { name, element_from_user( List, <<"name">> ) },
                                            { screen_name, element_from_user( List, <<"screen_name">> ) },
                                            { created, binary_to_list( Date ) }
                                           ]], reformat_friends_data( Rest, Service ))
                    end
            end
    end.

element_from_user( Message, Element ) ->
    case lists:keysearch( <<"user">>, 1, Message ) of
        { value, { <<"user">>, { struct, UserList } } } ->
            %io:format( "UserList: ~p~n", [ UserList ] ),
            case lists:keysearch( Element, 1, UserList ) of
                { value, { Element, User } } ->
                    binary_to_list( User )
            end
    end.

tfm( Message ) ->
    case lists:keysearch( created, 1, Message ) of
        { value, { created, Date } } ->
            case string:tokens( Date, " " ) of
                [ _, Month, Dom, Time, _, Year ] ->
                    Ic = fun(X) ->
                                 list_to_integer( X )
                         end,
                    [ H, M, S ] = string:tokens( Time, ":" ),
                    calendar:datetime_to_gregorian_seconds( { { Ic(Year), m_t_n( Month ), Ic(Dom) }, { Ic(H), Ic(M), Ic(S) } } );
                _ ->
                    io:format( "No time: ~p~n", [ Date ] ),
                    []
            end;
        _ ->
            io:format( "No Date? ~p~n", [ Message ] ),
            []
    end.

sort_messages( [] ) ->
    [];
sort_messages( [ H | T ] ) ->
    sort_messages( [ X || X <- T,
                          tfm( X ) < tfm( H ) ] ) ++ [ H ] ++ sort_messages( [ X || X <- T,
                                                                                    tfm( X ) >= tfm( H ) ] ).

m_t_n( Month ) ->
    Table = [
             { "Jan", 1 },
             { "Feb", 2 },
             { "Mar", 3 },
             { "Apr", 4 },
             { "May", 5 },
             { "Jun", 6 },
             { "Jul", 7 },
             { "Aug", 8 },
             { "Sep", 9 },
             { "Oct", 10 },
             { "Nov", 11 },
             { "Dec", 12 }
            ],
    { _, { _, Num } } = lists:keysearch( Month, 1, Table ),
    Num.


% Grabs the identifier for the current user.
auth_info( Arg ) ->
    H = Arg#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val( "dorkinator", C ) of
        [] ->
            { redirect, "/t/setup" };
        Cookie ->
            case yaws_api:cookieval_to_opaque( Cookie ) of
                { ok, Val } ->
                    case kvs:lookup( Val#session.key ) of
                        { ok, AuthInfo } ->
                            AuthInfo
                    end
            end
    end.
