
-module( u_handler ).
-export( [
          out/1
         ] ).
-include( "yaws_api.hrl" ).
-include( "dorkinator.hrl" ).

out( Pf ) ->
    Args = Pf:server_args(),
    Path = Args#arg.appmoddata,
    case Path of
        "u/login" ->
            login_handler( Args, Pf );
        _ ->
            login_handler( Args, Pf )
    end.

login_handler( A, Pf ) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            { html, Pf:page( "login" ) }
    end.
