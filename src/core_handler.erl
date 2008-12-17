
-module( core_handler ).
-export( [ out/1 ] ).
-include( "yaws_api.hrl" ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

out( A ) ->
    Path = A#arg.appmoddata,
    case regexp:first_match( Path, ".html$|.css$|.js$|.gif$|.jpg$" ) of
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
		".jpg" ->
		    { content, "image/jpeg", Px:static( Path ) };
		".html" ->
		    { content, "text/html", Px:static( Path ) }
	    end;
	nomatch ->
            case Path of
                "hello" ->
                    Px = pfactory:new( A ),
                    { html, Px:page( "hello" ) };
                "about" ->
                    Px = pfactory:new( A ),
                    { html, Px:page( "about" ) };
                "" ->
                    Px = pfactory:new( A ),
                    { html, Px:page( "login" ) };
                _ ->
                    case lists:nth( 1, string:tokens( Path, "/" ) ) of
                        "t" ->
                            t_handler:out( pfactory:new( A ) );
                        "u" ->
                            u_handler:out( pfactory:new( A ) );
                        _ ->
                            { redirect, "/" }
                    end
            end
    end.
