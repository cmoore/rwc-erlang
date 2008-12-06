
-module( pfactory, [ SArgs ] ).
-export( [ page/1, page/2, static/1, server_args/0 ] ).
-license( { mit_license, "http://www.linfo.org/mitlicense.html" } ).

-include( "yaws_api.hrl" ).

static( File ) ->
    { ok, Contents } = file:read_file( "www/" ++ File ),
    Contents.

% spage = string page.
% pass in a string and get back a rendered template.        
page( String ) ->
    page( String, [] ).

page( String, Obj ) ->
    Tatom = list_to_atom( String ),
    case Tatom:render( template_args( Obj ) ) of
        { has_errors, false } ->
            page( "catastrophic" );
        { ok, Content } ->
            Content;
        _ ->
            ""
    end.

template_args( Params ) ->
    case lists:keysearch( error, 1, Params ) of
        { value, { _, _ } } ->
            lists:merge( Params, [ { has_errors, true } ] );
        _ ->
            lists:merge( Params, [ { has_errors, false } ] )
    end.

server_args() ->
    SArgs.
