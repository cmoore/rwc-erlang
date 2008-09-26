
-module( dorkinator ).
-export( [
          start/0,
          validate/3,
          stop/0,
          halt/0,
          s_init/0,
          s_store/2,
          s_find/1,
          build_templates/0,
          single_message/0,
          init_database/0,
          rebuild_tables/0
         ] ).

-include( "dorkinator.hrl" ).
-include( "yaws.hrl" ).
-include( "yaws_api.hrl" ).
-include_lib( "stdlib/include/qlc.hrl" ).

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
    GC = yaws_config:make_default_gconf( false, "" ),
    SC = #sconf{ port = 3000,
                 servername = "localhost",
                 listen = { 0,0,0,0 },
                 docroot = "www",
                 appmods = [ { "/", core_handler } ]
                },
    kvs:start(),
    mnesia:start(),
    % mnesia:wait_for_tables( [  services, users ], 2000 ),
    erlydtl:create_parser(),
    build_templates(),
    yaws_api:setconf( GC, [[ SC ]] ).

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

s_init() ->
    Px = ets:new( ipdsessions, [ set, public, named_table ] ),
    ets:insert( Px, { "the_first_record", "must_be_inserted_for_some_reason" } ).

s_store( Login, Auth ) ->
    ets:insert( ipdsessions, { Login, Auth } ).

s_find( Login ) ->
    case ets:lookup( ipdsessions, Login ) of
        [ { Login, Auth } ] ->
            Auth;
        _ ->
            false
    end.        

build_templates() ->
    TemplateList = [ "login", "tweet", "header", "footer", "index", "catastrophic","setup", "qdirect", "viewer" ],
    [ erlydtl_compiler:compile( "./templates/" ++ X ++ ".html", X, [ { out_dir, "./ebin" } ] ) || X <- TemplateList ].

single_message() ->
    case lwtc:setup( [ { login, "hydo" }, { password, "m1n3rv4" } ] ) of
        { ok, Id } ->
            [ Px | _ ] = lwtc:request( Id, friends_timeline ),
            Px;
        _ ->
            false
    end.

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
