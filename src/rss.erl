
-module( rss ).
-export( [ parse/1 ] ).
-include_lib( "xmerl/include/xmerl.hrl" ).

parse( Url ) ->
    case http:request( get, { Url, [] }, [], [] ) of
        { ok, { _, _, Body } } ->
            { Xml, _Rest } = xmerl_scan:string(Body),
            format_entries(xmerl_xpath:string("//entry",Xml))
    end.

format_entries([]) -> done;
format_entries([Node|Rest]) ->
    [ #xmlText{value=Title} ] = xmerl_xpath:string("title/text()", Node),
    [ #xmlAttribute{value=Link} ] = xmerl_xpath:string("link/@href", Node),
    Message = xmerl:export_simple_content([{a,[{href,Link}],[Title]}],xmerl_xml),
    io:format('~s~n', [xmerl_ucs:to_utf8(Message)]),
    format_entries(Rest).
