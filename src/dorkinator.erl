
-module( dorkinator ).
-export( [
	  start/0,
	  validate/3,
	  stop/0,
	  halt/0,
	  s_init/0,
	  s_store/2,
	  s_find/1,
	  build_templates/0
	 ] ).

-include( "/usr/local/lib/yaws/include/yaws.hrl" ).
-include( "/usr/local/lib/yaws/include/yaws_api.hrl" ).
-include_lib( "stdlib/include/qlc.hrl" ).

stop() ->
    application:stop( yaws ),
    mnesia:stop(),
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
		 listen = { 127,0,0,1 },
		 docroot = "www",
		 appmods = [ { "/", core_handler } ]
		},
    kvs:start(),
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
    TemplateList = [ "header", "footer", "index", "catastrophic","setup", "qdirect", "viewer" ],
    [ erlydtl_compiler:compile( "./templates/" ++ X ++ ".html", X, [ { out_dir, "./ebin" } ] ) || X <- TemplateList ].
