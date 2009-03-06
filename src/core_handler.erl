
-module( core_handler ).
-export( [ out/1 ] ).
-include( "yaws_api.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

out( A ) ->
    Path = A#arg.appmoddata,
    case regexp:first_match( (A#arg.headers)#headers.host, "p.hellyeah.org" ) of
        { match, _, _ } ->
            { redirect, "http://rwc.hellyeah.org/" };
        _ ->
            case regexp:first_match( Path, ".zip|.tar.gz|.gz|.avi$|.html$|.css$|.js$|.gif$|.jpg$" ) of
                { match, X, Y } ->
                    Px = pfactory:new( A ),
                    % I know there's a better way to do this but I couldn't get
                    % vdirs working in the yaws config.
                    case string:substr( Path, X, Y ) of
                        ".avi" ->
                            { content, "x-msvideo", Px:static( Path ) };
                        ".css" ->
                            { content, "text/css", Px:static( Path ) };
                        ".js" ->
                            { content, "text/javascript", Px:static( Path ) };
                        ".gif" ->
                            { content, "image/gif", Px:static( Path ) };
                        ".tar.gz" ->
                            { content, "application/x-gzip", Px:static( Path ) };
                        ".zip" ->
                            { content, "application/zip", Px:static( Path ) };
                        ".jpg" ->
                            { content, "image/jpeg", Px:static( Path ) };
                        ".html" ->
                            { content, "text/html", Px:static( Path ) }
                    end;
                nomatch ->
                    % At this point, the request path does not end in a file suffix that we want to serve statically.
                    case Path of
                        % TODO - it should be relatively simple to create a mapping in something like YAML
                        % TODO - to map paths to handlers.
                        "viewer" ->
                            t_handler:viewer_handler( A );
                        "delete_service" ->
                            t_handler:delete_service( A );
                        "setup" ->
                            t_handler:setup_handler( A );
                        "post" ->
                            t_handler:tweet_handler( A );
                        "login" ->
                            u_handler:login_handler( A );
                        "logout" ->
                            u_handler:logout_handler( A );
                        "register" ->
                            u_handler:register_handler( A );
                        "geo_setup" ->
                            t_handler:geo_menu( A );
                        "geo_browse_address" ->
                            t_handler:set_location( A );
                        "about" ->
                            Px = pfactory:new( A ),
                            { html, Px:page( "about" ) };
                        "" ->
                            Px = pfactory:new( A ),
                            { html, Px:page( "login" ) };
                        _ ->
                            io:format( "Unhandled request: ~p~n", [ Path ] ),
                            { redirect, "/" }
                    end
            end
    end.
