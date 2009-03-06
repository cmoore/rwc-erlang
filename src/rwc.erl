
-module( rwc ).
-export( [
          start/0,
          validate/3,
          stop/0,
          halt/0,
          gen_key/0,
          auth_info/1,
          build_templates/0,
          init_database/0,
          rebuild_tables/0,
          format_cookie/1,
          hexdigest/1,
          exp_string/0,
          pmap/2
         ] ).

-include( "rwc.hrl" ).
-include( "yaws.hrl" ).
-include( "yaws_api.hrl" ).
-include_lib( "stdlib/include/qlc.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

stop() ->
    application:stop( yaws ),
    inets:stop(),
    crypto:stop().

halt() ->
    stop(),
    init:stop().

start() ->
    make:all(),
    crypto:start(),
    inets:start(),
    application:start( yaws ),
    application:start( ecouch ),
    GC = yaws_config:make_default_gconf( false, "" ),
    SC = #sconf{ port = 3000,
                 servername = "localhost",
                 listen = { 0,0,0,0 },
                 docroot = "www",
                 appmods = [ { "/", core_handler } ]
                },
    mnesia:start(),
    % mnesia:wait_for_tables( [  services, users ], 2000 ),
    erlydtl:create_parser(),
    build_templates(),
    yaws_api:setconf( GC, [[ SC ]] ).

build_templates() ->
    TemplateList = [ "hello", "geo_setup", "toolbar", "register",
                     "about", "login", "tweet", "header", "footer",
                     "index", "catastrophic","setup", "qdirect", "viewer" ],
    [ erlydtl_compiler:compile( "./templates/" ++ X ++ ".html", X, [ { out_dir, "./ebin" } ] ) || X <- TemplateList ].

rebuild_tables() ->
    mnesia:delete_table( users ),
    mnesia:delete_table( services ),
    init_database().

init_database() ->
    mnesia:stop(),
    mnesia:create_schema( [ node() ] ),
    mnesia:start(),
    mnesia:create_table( users, [
                                 { disc_copies, [ node() ] },
                                 { attributes, record_info( fields, users ) }
                                ] ),
    mnesia:create_table( services, [
                                  { disc_copies, [ node() ] },
                                  { attributes, record_info( fields, services ) }
                                 ] ).

%
%
% Web Utils
%
%

validate(A, Fields, Fun) ->
    Params = yaws_api:parse_post(A),
    lists:foldl(
      fun(Field, {Vals, Errs}) ->
              case proplists:lookup(Field, Params) of
                  none -> exit({missing_param, Field});
                  {_, Val} ->
                      Val1 = case Val of undefined -> ""; _ -> Val end,
                      Acc1 =
                          case Fun(Field, Val1) of
                              ok ->
                                  {[Val1 | Vals], Errs};
                              Err ->
                                  {[Val1 | Vals], [Err | Errs]}
                          end,
                      Acc1
              end
      end, {[], []}, lists:reverse(Fields)).





format_cookie( Px ) ->
    Cookie = yaws_api:new_cookie_session( Px, 259200 ),
    yaws_api:setcookie( "dorkinator", Cookie, "/", exp_string() ).

gen_key() ->
    Key = crypto:rand_bytes( 20 ),
    base64:encode( binary_to_list( Key ) ).

% Grabs the identifier for the current user.
auth_info( Arg ) ->
    H = Arg#arg.headers,
    C = H#headers.cookie,
    case yaws_api:find_cookie_val( "dorkinator", C ) of
        [] ->
            io:format( "find_cookie_val failed." ),
            false;
        Cookie ->
            case yaws_api:cookieval_to_opaque( Cookie ) of
                { ok, { session, Val } } ->
                    case users:auth_confirm( Val ) of
                        false ->
                            io:format( "auth_confirm failed." ),
                            false;
                        Px ->
                            [ Vt | _ ] = Px,
                            Vt
                    end;
                { error, no_session } ->
                    io:format( "cookieval_to_opaque failed." ),
                    false
            end
    end.

hexdigest( Px ) ->
    lists:flatten( lists:map( fun( V ) ->
                                      httpd_util:integer_to_hexlist( V ) end,
                              binary_to_list( erlang:md5( Px ) )
                              ) ).

exp_string() ->
    { { _, Month, Day }, Time } = erlang:localtime(),
    httpd_util:rfc1123_date( { { 2030, Month, Day }, Time } ).

map( _, [] ) ->
    [];
map( F, [ H | T ] ) ->
    [ F( H ) | map( F, T ) ].

pmap( F, L ) ->
    S = self(),
    Ref = erlang:make_ref(),
    Pids = map( fun( I ) ->
                        spawn( fun() ->
                                       do_f( S, Ref, F, I ) end )
                end, L ),
    gather( Pids, Ref ).

do_f( Parent, Ref, F, I ) ->
    Parent ! { self(), Ref, ( catch F(I))}.

gather( [ Pid | T ], Ref ) ->
    receive
        { Pid, Ref, Ret } ->
            [ Ret | gather( T, Ref ) ]
    end;
gather( [], _ ) ->
    [].
