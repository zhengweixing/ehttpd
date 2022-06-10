-author("kenneth").
-define(APP, ehttpd).
-define(SWAGGER(Name, Ver), atom_to_list(Name) ++ "_" ++ binary_to_list(Ver) ++ ".json").
-define(ACCESS_CONTROL_ALLOW_HEADERS, list_to_binary(application:get_env(ehttpd, access_control_allow_headers, "false"))).


%% 默认请求头
-define(HEADER, #{
    <<"server">> => <<"API Server">>,
    <<"access-control-allow-origin">> => <<"*">>,
    <<"access-control-allow-credentials">> => <<"true">>,
    <<"access-control-allow-methods">> => <<"GET, POST, OPTIONS, PUT, DELETE">>
}).