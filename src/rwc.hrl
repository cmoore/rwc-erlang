
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

-record( session, { key } ).
-record( users, { login, password, service_key, auth, email, active } ).
-record( services, { idx, username, password, service, service_key } ).
