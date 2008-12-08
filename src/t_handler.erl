
-module( t_handler ).
-export( [ out/1,
           tfm/1,
           reformat_friends_data/2,
           reformat_services/1,
           remove_non_twitter/1,
           urlize/2,
           sort_messages/1  ] ).
-include( "yaws_api.hrl" ).
-include( "dorkinator.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

out( Pf ) ->
    A = Pf:server_args(),
    Path = A#arg.appmoddata,
    case Path of
        "t/public" ->
            public_handler( A, Pf );
        "t/direct" ->
            direct_handler( A, Pf );
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
            case( A#arg.req )#http_request.method of
                'GET' ->
                    { redirect, "/t/viewer" };
                'POST' ->
                    Args = [ { list_to_atom( X ),Y } || {X,Y}<- yaws_api:parse_post( A ) ],
                    case lists:keysearch( message, 1, Args ) of
                        { value, { message, Message } } ->
                            case lists:keysearch( post_identica, 1, Args ) of
                                { value, { _, _ } } ->
                                    Pc = services:cred_for_service( Px#users.login, "identica" ),
                                    lwtc:update( Pc, Message );
                                _ ->
                                    none
                            end,
                            case lists:keysearch( post_twitter, 1, Args ) of
                                { value, { _, _ } } ->
                                    Rc = services:cred_for_service( Px#users.login, "twitter" ),
                                    lwtc:update( Rc, Message );
                                _ ->
                                    none
                            end
                    end,
                    { redirect, "/t/viewer" }
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

validate_field( "account_type", _ ) ->
    ok;
validate_field( "acct_login", Login ) when length( Login ) > 1 ->
    ok;
validate_field( "acct_login", _ ) ->
    { error, "Value is too short." };

validate_field( "acct_password", Pass ) when length( Pass ) > 2 ->
    ok;
validate_field( "acct_Password", _ ) ->
    { error, "Value is too short." };

validate_field( "svc", Line ) when length( Line ) > 1 ->
    ok;
validate_field( "svc", _ ) ->
    { error, "Value is too short." }.

rfmt( [], _Type ) ->
    [];
rfmt( [ Svc | Rst ], Type ) when Type == direct_messages ->
    case lwtc:nrequest( Svc#services.username, Svc#services.password, Svc#services.service, Type ) of
        { error, _ } ->
            reformat_direct_messages( [], Svc#services.service ) ++ rfmt( Rst, Type );
        MList ->
            reformat_direct_messages( MList, Svc#services.service ) ++ rfmt( Rst, Type )
    end;            
rfmt( [ Svc | Rst ], Type ) ->
    case pull_service_data(
           Svc#services.username,
           Svc#services.password,
           Svc#services.service,
           Type ) of
        {error, lwtc_errored_out} ->
            reformat_friends_data( [], Svc#services.service ) ++ rfmt( Rst, Type );
        Px ->
            reformat_friends_data( Px, Svc#services.service ) ++ rfmt( Rst, Type )
    end.

remove_non_twitter( [] ) ->
    [];
remove_non_twitter( [ Sv | Rst ] ) ->
    case Sv#services.service of
        "twitter" ->
            lists:append( [ [ Sv ] ], remove_non_twitter( Rst ) );
        _ ->
            lists:append( [ [ ] ], remove_non_twitter( Rst ) )
    end.

public_handler( A, Px ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            Msg = [ reformat_friends_data(lwtc:nrequest( X#services.username, X#services.password, X#services.service, public_timeline ), X#services.service ) ||
                      X <- services:by_user( Vx#users.login ) ],
            { html, Px:page( "viewer", [ { twittermessages, lists:reverse( Msg ) } ] ) }
    end.

direct_handler( A, Px ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            Msg = [ lwtc:nrequest( X#services.username, X#services.password, X#services.service, direct_messages ) ||
                      X <- lists:flatten( remove_non_twitter( services:by_user( Vx#users.login ) ) ) ],
            { html, Px:page( "viewer", [ { twittermessages, lists:reverse( Msg ) } ] ) }
    end.

viewer_handler( A, Px ) ->
    case dorkinator:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            All_Messages = rfmt( services:by_user( Vx#users.login ), replies ) ++
                rfmt( services:by_user( Vx#users.login ), friends_timeline ) ++
                rfmt( services:by_user( Vx#users.login ), direct_messages ),
            { html, Px:page( "viewer", [ { twittermessages, lists:reverse( sort_messages( All_Messages ) ) } ] ) }
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
    { struct, List } = Element,
    { value, { <<"id">>, Id } } = lists:keysearch( <<"id">>, 1, List ),
    { value, { <<"text">>, Text } } = lists:keysearch( <<"text">>, 1, List ),
    { value, { <<"created_at">>, Date } } = lists:keysearch( <<"created_at">>, 1, List ),

    [ [
       { svc, Service },
       { id, Id },
       { text, urlize( Text, Service ) },
       { picture, element_from_user( List, <<"profile_image_url">> ) },
       { name, element_from_user( List, <<"name">> ) },
       { screen_name, element_from_user( List, <<"screen_name">> ) },
       { created, binary_to_list( Date ) }
      ] ] ++ reformat_friends_data( Rest, Service ).

reformat_direct_messages( [], _Service ) ->
    [];
reformat_direct_messages( [ Element_Raw | Rest ], Service ) ->
    { struct, Element } = Element_Raw,
    { value, { _, { struct, Sender } } } = lists:keysearch( <<"sender">>, 1, Element ),
    { value, { _, BName } } = lists:keysearch( <<"name">>, 1, Sender ),
    { value, { _, SName } } = lists:keysearch( <<"screen_name">>, 1, Sender ),
    { value, { _, Message } } = lists:keysearch( <<"text">>, 1, Element ),
    { value, { _, Date } } = lists:keysearch( <<"created_at">>, 1, Element ),
    { value, { _, Picture } } = lists:keysearch( <<"profile_image_url">>, 1, Sender ),

    [[
      { svc, Service },
      { name, binary_to_list( BName ) },
      { text, binary_to_list( Message ) },
      { created, binary_to_list( Date ) },
      { picture, binary_to_list( Picture ) },
      { screen_name, binary_to_list( SName ) },
      { type, "direct" }
     ]] ++ reformat_direct_messages( Rest, Service ).

element_from_user( Message, Element ) ->
    { value, { <<"user">>, { struct, UserList } } } = lists:keysearch( <<"user">>, 1, Message ),
    { value, { Element, User } } = lists:keysearch( Element, 1, UserList ),
    binary_to_list( User ).

tfm( Message ) ->
    { value, { created, Date } } = lists:keysearch( created, 1, Message ),
    [ _, Month, Dom, Time, _, Year ] = string:tokens( Date, " " ),
    Ic = fun( X ) ->
                 list_to_integer( X )
         end,
    [ H, M, S ] = string:tokens( Time, ":" ),
    calendar:datetime_to_gregorian_seconds( { { Ic(Year), m_t_n( Month ), Ic(Dom) }, { Ic(H), Ic(M), Ic(S) } } ).

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
    Urlized = case regexp:match( Vx, "^http://" ) of
                   { match, _, _ } ->
                       "<a href=\"" ++ Vx ++ "\">" ++ Vx ++ "</a>";
                   _ ->
                       Vx
               end,
    case regexp:match( Urlized, "^#" ) of
        { match, _, _ } ->
            case Service of
                "identica" ->
                    { ok, Fx, _ } = regexp:sub( Urlized, "^#", "" ),
                    "<a href=\"http://identi.ca/tag/" ++ Fx ++ "\">" ++ Urlized ++ "</a>";
                _ ->
                    Urlized
            end;
        _ ->
            Urlized
    end.

reformat_services( [] ) ->
    [];
reformat_services( [ Px | Rst ] ) ->
    { services, Idx, Login, _, Service, _ } = Px,
    [ [ { idx, Idx }, { login, Login }, { service, Service } ] ] ++ reformat_services( Rst ).

