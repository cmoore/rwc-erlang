
-module( t_handler ).
-export( [ out/1,
           tfm/1,
           reformat_friends_data/2,
           reformat_services/1,
           urlize/2,
           sort_messages/1  ] ).
-include( "yaws_api.hrl" ).
-include( "dorkinator.hrl" ).

out( Pf ) ->
    A = Pf:server_args(),
    Path = A#arg.appmoddata,
    case Path of
        "t/delete_service" ->
            delete_service( A, Pf );
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
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Px ->
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
                            case Tpost of
                                "on" ->
                                    Rc = services:cred_for_service( Px#users.login, "twitter" ),
                                    lwtc:update( Rc, Message ),
                                    { redirect, "/t/viewer" }
                            end
                    end
            end
    end.

delete_service( A, _Pf ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        _Px ->
            case ( A#arg.req)#http_request.method of
                'GET' ->
                    { redirect, "/t/setup" };
                'POST' ->
                    case dorkinator:validate( A, [ "svc" ], fun validate_field/2 ) of
                        { [ Service ], [] } ->
                            services:delete( Service ),
                            { redirect, "/t/setup" }
                    end
            end
    end.

setup_handler( A, Pf ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Info ->
            case (A#arg.req)#http_request.method of
                'GET' ->
                    { html, Pf:page( "setup", [
                                               { services, reformat_services( services:by_user( Info#users.login ) ) }
                                              ] ) };
                'POST' ->
                    case dorkinator:validate( A, [
                                                  "account_type",
                                                  "acct_login",
                                                  "acct_password"
                                                 ], fun validate_field/2 ) of
                        { [ Type, Login, Password ], [] } ->
                            services:add_service( Info#users.login, Login, Password, Type ),
                            { html, Pf:page( "setup", [
                                                       { services, reformat_services( services:by_user( Info#users.login ) ) }
                                                      ] ) }
                    end
            end
    end.

validate_field( _X, _Y ) ->
    ok.

sym( Svc ) ->
    case Svc of
        "identi.ca" ->
            "identica";
        _ ->
            Svc
    end.

rfmt( [] ) ->
    [];
rfmt( [ Svc | Rst ] ) ->
    lists:append( reformat_friends_data( pull_service_data(
                                           Svc#services.username,
                                           Svc#services.password,
                                           sym(Svc#services.service),
                                           friends_timeline ), sym( Svc#services.service ) ), rfmt( Rst ) ).
viewer_handler( A, Px ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            All_Messages = sort_messages( rfmt( services:by_user( Vx#users.login ) ) ),
            { html, Px:page( "viewer", [
                                        { twittermessages, lists:reverse( All_Messages ) } ] ) }
    end.

pull_service_data( Login, Password, Service, Request ) ->
    Px = lwtc:nrequest( Login, Password, Service, Request ),
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
                                            { svc, Service },
                                            { id, Id },
                                            { text, urlize(Text, Service) },
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
                    []
            end;
        _ ->
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


urlize( Message, Service ) ->
    list_to_binary(
      string:join( [ yoorl( X, Service ) || X <- string:tokens( binary_to_list(Message), " " ) ], " " )
     ).

yoorl( X, Service ) ->
    Vx = case regexp:match( X, "^@" ) of
             { match, _, _ } ->
                 { ok, Tx, _ } = regexp:sub( X, "^@", "" ),
                 Fv = case regexp:sub( Tx, ":$", "" ) of
                          { badmatch, _, _ } ->
                              Tx;
                          { ok, Gl, _ } ->
                              Gl
                      end,
                 case Service of
                    "twitter" ->
                         "<a href=\"http://www.twitter.com/" ++ Fv ++ "\">" ++ X ++ "</a>";
                     "identica" ->                                   
                         "<a href=\"http://identi.ca/" ++ Fv ++ "\">" ++ X ++ "</a>"
                 end;
             _ ->
                 X
         end,
    case regexp:match( Vx, "^http://" ) of
        { match, _, _ } ->
            "<a href=\"" ++ Vx ++ "\">" ++ Vx ++ "</a>";
        _ ->
            Vx
    end.

reformat_services( [] ) ->
    [];
reformat_services( [ Px | Rst ] ) ->
    case Px of 
        { services, Idx, Login, _, Service, _ } ->
            lists:append( [[ { idx, Idx }, { login, Login }, { service, Service } ]], reformat_services( Rst ) )
    end.
