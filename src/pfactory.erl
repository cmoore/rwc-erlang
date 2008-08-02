
-module( pfactory, [ SArgs ] ).
-export( [ spage/1, spage/2, page/1, page/2, render/1, render/2, template/1, static/1, server_args/0 ] ).

-include( "yaws_api.hrl" ).

% spage = string page.
% pass in a string and get back a rendered template.	    
spage( String ) ->    
    spage( String, [] ).

spage( String, Obj ) ->
    { ok, Out } = sgte:compile( String ),
    render( Out, Obj ).

page( Page ) ->
    render( template( Page ) ).

page( Page, Args ) ->
    render( template( Page ), Args ).

render( Contents ) ->
    render( Contents, [] ).

render( Contents, Params ) ->
    Px = template_args( Params ),
    sgte:render( Contents, Px ).


template( File ) ->
    { ok, Contents } = sgte:compile_file( "templates/" ++ File ),
    Contents.

static( File ) ->
    { ok, Contents } = file:read_file( "www/" ++ File ),
    Contents.

template_args( Params ) ->
    case lists:keysearch( error, 1, Params ) of
	{ value, { _, _ } } ->
	    lists:merge( Params, [ { has_errors, true }, { header, template( "header.html" ) }, { footer, template( "footer.html" ) } ] );
	_ ->
	    lists:merge( Params, [ { has_errors, false }, { header, template( "header.html" ) }, { footer, template( "footer.html" ) } ] )
    end.

server_args() ->
    SArgs.
