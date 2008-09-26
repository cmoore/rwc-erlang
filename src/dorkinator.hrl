
-ifdef( TEST ).
-include_lib( "eunit/include/eunit.hrl" ).
-endif.

-record( session, { key } ).
-record( users, { service_key, login, password } ).
-record( services, { username, password, service, service_key } ).

