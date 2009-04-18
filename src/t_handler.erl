
-module( t_handler ).
-export( [ tfm/1,
           set_location/1,
           geo_menu/1,
           reformat_friends_data/2,
           reformat_services/1,
           remove_non_twitter/1,
           urlize/2,
           near_me/1,
           tweet_handler/1,
           shift_to_twitter/1,
           un_punctuate/1,
           delete_service/1,
           viewer_handler/1,
           setup_handler/1,
           sort_messages/1  ] ).
-include( "yaws_api.hrl" ).
-include( "rwc.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

%
% I need to figure out something nice and elegant to get rid of these case nests.
% It's really unsightly.
%
tweet_handler( A ) ->
    case rwc:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Px ->
            case( A#arg.req )#http_request.method of
                'GET' ->
                    { redirect, "/viewer" };
                'POST' ->
                    Args = [ { list_to_atom( X ), Y } || { X , Y } <- yaws_api:parse_post( A ) ],
                    case lists:keysearch( message, 1, Args ) of
                        { value, { message, Message } } ->
                            case lists:keysearch( post_identica, 1, Args ) of
                                { value, { _, _ } } ->

                                    % I'm still not completely convinced that creating a temporary variable
                                    % is cleaner then having one function inside another.
                                    % The indenting helps though.

                                    %Pc = services:cred_for_service( Px#users.login, "identica" ),
                                    
                                    lwtc:update(
                                      (services:cred_for_service( Px#users.login, "identica" )),
                                      Message );
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
                    { redirect, "/viewer" }
            end
    end.

delete_service( A ) ->
    case rwc:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        _Px ->
            case ( A#arg.req)#http_request.method of
                'GET' ->
                    { redirect, "/setup" };
                'POST' ->
                    case rwc:validate( A, [ "svc" ], fun validate_field/2 ) of
                        { [ Service ], [] } ->
                            services:delete( Service ),
                            { redirect, "/setup" }
                    end
            end
    end.

setup_handler( A ) ->
    case rwc:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Info ->
            case (A#arg.req)#http_request.method of
                'GET' ->
                    Pf = pfactory:new( A ),
                    { html, Pf:page( "setup", [
                                               { services, reformat_services( services:by_user( Info#users.login ) ) }
                                              ] ) };
                'POST' ->
                    case rwc:validate( A, [
                                                  "account_type",
                                                  "acct_login",
                                                  "acct_password"
                                                 ], fun validate_field/2 ) of
                        { [ Type, Login, Password ], [] } ->
                            services:add_service( Info#users.login, Login, Password, Type ),
                            Pf = pfactory:new( A ),
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

validate_field( "longitude", _ ) ->
    ok;
validate_field( "latitude", _ ) ->
    ok;

validate_field( "svc", Line ) when length( Line ) > 1 ->
    ok;
validate_field( "svc", _ ) ->
    { error, "Value is too short." }.

rfmt( [], _Type ) ->
    [];
rfmt( [ Svc | _Rst ], Type ) when Type == near_me ->
    case lwtc:request( Svc#services.username, Svc#services.password, Svc#services.service, Type ) of
        { error, _ } ->
            [];
        MList ->
            reformat_friends_data( MList, Svc#services.service )
    end;
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

near_me( A ) ->
    case rwc:auth_info( A ) of
        false ->
            { redirect, "/login" };
        Vx ->
            Key = Vx#users.login ++ "-loc",
            case lwtc:keyd_lookup( Key ) of
                { ok, Geocode } ->
                    Px = pfactory:new( A ),
                    case shift_to_twitter( services:by_user( Vx#users.login ) ) of
                        [] ->
                            { redirect, "/geo_setup" };
                        Info ->
                            { struct, List } = lwtc:near_me( Info#services.username, Info#services.password, Geocode ),
                            { value, { <<"results">>, Messages } } = lists:keysearch( <<"results">>, 1, List ),
                            { html, Px:page( "viewer", [ { twittermessages, sort_geo_messages( Messages ) } ] ) }
                    end
            end
    end.

viewer_handler( A ) ->
    case rwc:auth_info( A ) of
        false ->
            { redirect, "/u/login" };
        Vx ->
            Px = pfactory:new( A ),
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

% Convert the time to something that a little easier to read.
% I was probably thinking "time format" when I made this.
tfm( Message ) ->
    { value, { created, Date } } = lists:keysearch( created, 1, Message ),
    [ _, Month, Dom, Time, _, Year ] = string:tokens( Date, " " ),
    Ic = fun( X ) ->
                 list_to_integer( X )
         end,
    [ H, M, S ] = string:tokens( Time, ":" ),
    calendar:datetime_to_gregorian_seconds( { { Ic(Year), m_t_n( Month ), Ic(Dom) }, { Ic(H), Ic(M), Ic(S) } } ).

% Sorts the messages by time as returned by tfm()
%
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

%
% The text portion of the tweet can come with all sorts of funky stuff in it.
% Well, at least it seems funky when you are dealing with regexes in a new language.
% eg.
% @user omg hai!
% @user, lookie here: http://x.com/!
%     ^comma                       ^ exclamation mark
% All of these play hell with my hastily implemented reformatting.
%
urlize( Message, Service ) ->
    list_to_binary(
      string:join( [ un_punctuate( yoorl( X, Service ) ) || X <- string:tokens( binary_to_list(Message), " " ) ], " " )
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
    Urlized = case regexp:match( Vx, "^http:" ) of
                   { match, _, _ } ->
                       "<a href=\"" ++ Vx ++ "\">" ++ Vx ++ "</a>";
                   _ ->
                       Vx
               end,
    case regexp:match( Urlized, "^#" ) of
        { match, _, _ } ->
            case Service of
                "twitter" ->
                    "<a href=\"http://search.twitter.com/search?q=" ++ yaws_api:url_encode(Urlized) ++ "\">" ++ Urlized ++ "</a>";
                "identica" ->
                    
                    { ok, NoUnderscore, _ } = regexp:sub( Urlized, "_", "" ),
                    { ok, Fx, _ } = regexp:sub( NoUnderscore, "^#", "" ),
                    { ok, Px, _ } = regexp:sub( Fx, "\\.$", "" ),
                    "<a href=\"http://identi.ca/tag/" ++ string:to_lower(Px) ++ "\">" ++ Urlized ++ "</a>";
                _ ->
                    Urlized
            end;
        _ ->
            Urlized
    end.

% Ah, much better than what was here before.
un_punctuate( Word ) ->
    lists:foldl( fun( X, Elem ) ->
                         { ok, Rv, _ } = regexp:gsub( Elem, X, "" ),
                         Rv
                 end, Word, [ "!", "," ] ).

reformat_services( [] ) ->
    [];
reformat_services( [ Px | Rst ] ) ->
    { services, Idx, Login, _, Service, _ } = Px,
    [ [ { idx, Idx }, { login, Login }, { service, Service } ] ] ++ reformat_services( Rst ).

%% Begin the geo stuff.

geo_menu( Args ) ->
    Page = pfactory:new( Args ),
    case ( Args#arg.req )#http_request.method of
        'GET' ->
            { html, Page:page( "geo_setup" ) };
        'POST' ->
            { html, Page:page( "geo_setup", [ { error, "In post" } ] ) }
    end.

set_location( Args ) ->
    case rwc:validate( Args, [ "longitude", "latitude" ], fun validate_field/2 ) of
        { [ Lon, Lat ], [] } ->
            Ts = Lon ++ "," ++ Lat,
            Fx = rwc:auth_info( Args ),
            Dx = Fx#users.login ++ "-loc",
            lwtc:keyd_store( Dx, Ts ),
            { redirect, "/near_me" };
        _ ->
            { redirect, "/geo_setup" }
    end.

shift_to_twitter( [] ) ->
    [];
shift_to_twitter( [ Px | Rest ] ) ->
    case Px#services.service of
        "twitter" ->
            Px;
        _ ->
            shift_to_twitter( Rest )
    end.

sort_geo_messages( [ Message | Rest ] ) ->
    { struct, List } = Message,
    { value, { <<"location">>, Loc } } = lists:keysearch( <<"location">>, 1, List ),
    case binary_to_list( Loc ) of
	"GLOBAL" ->
		sort_geo_messages( Rest );
        "Earth" ->
            sort_geo_messages( Rest );
        "Hub" ->
            sort_geo_messages( Rest );
        _ ->
            { value, { <<"text">>, Text } } = lists:keysearch( <<"text">>, 1, List ),
            { value, { <<"from_user_id">>, Userid }} = lists:keysearch( <<"from_user_id">>, 1, List ),
            { value, { <<"from_user">>, Fromuser }} = lists:keysearch( <<"from_user">>, 1, List ),
            { value, { <<"profile_image_url">>, Picture }} = lists:keysearch( <<"profile_image_url">>, 1, List ),
            { value, { <<"created_at">>, Created }} = lists:keysearch( <<"created_at">>, 1, List ),
            { value, { <<"id">>, Id }} = lists:keysearch( <<"id">>, 1, List ),
            [ [ 
                { id, Id },
                { svc, "twitter" },
                { text, urlize( Text, "twitter" ) },
                { picture, binary_to_list( Picture ) },
                { created, binary_to_list( Created ) },
                { user_id, Userid },
                { screen_name, binary_to_list( Fromuser ) },
                { name, binary_to_list( Fromuser ) },
                { type, "geo" }
               ] ] ++ sort_geo_messages( Rest )
    end;

sort_geo_messages( [] ) ->
    [].

