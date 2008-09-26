
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

-record( session, { key } ).
-record( users, { login, password, service_key, auth } ).
-record( services, { username, password, service, service_key } ).
