-module(system_controller).
-behavior(ehttpd_rest).
-ehttpd_rest(default).

%% API
-export([swagger/1, handle/4]).

swagger(default) ->
    [
        ehttpd_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
    ].


-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Data :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Data :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Data :: map(), Req :: ehttpd_req:req()}.

handle(OperationID, Args, Context, _Req) ->
    case do_request(OperationID, Args, Context) of
        {error, Reason} ->
            Err = list_to_binary(io_lib:format("~p", [Reason])),
            {500, #{error => Err}};
        {Status, Headers, Res} ->
            {Status, Headers, Res};
        {Status, Res} ->
            {Status, Res}
    end.


do_request(post_generate_api, #{<<"mod">> := Mod} = Args, _Context) ->
    SWSchema = maps:without([<<"mod">>], Args),
    case ehttpd_swagger:compile(default, binary_to_atom(Mod), SWSchema) of
        {ok, Module, Src} when is_binary(Src) ->
            FileName = "ehttpd_plugin.zip",
            SrcPath = lists:concat(["handler/", Module, ".erl"]),
%%            SchemaPath = binary_to_list(<<"swagger/swagger_", Mod/binary, ".json">>),
            case create_zip(FileName, [
%%                {"ehttpd_plugin/priv/" ++ SchemaPath, jiffy:encode(SWSchema)},
                {"ehttpd_plugin/src/" ++ SrcPath, Src}
            ]) of
                {ok, ZipFile} ->
                    Headers = #{
                        <<"content-type">> => <<"application/zip">>,
                        <<"Content-Disposition">> => list_to_binary("attachment;filename=" ++ FileName)
                    },
                    {200, Headers, ZipFile};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;


do_request(post_upload, #{<<"file">> := FileInfo}, Context) ->
    {ok, FileInfo1} = ehttpd_hook:run('file.upload', [Context], FileInfo),
    {200, FileInfo1};

do_request(get_nodes, Args, _Context) ->
    Info = get_nodes_info(get_node(Args)),
    {200, #{data => Info, code => 200}};

do_request(get_brokers, Args, _Context) ->
    Info = get_brokers(get_node(Args)),
    {200, #{data => Info, code => 200}};

do_request(get_stats, Args, _Context) ->
    Info = get_stats(get_node(Args)),
    {200, #{data => Info, code => 200}};

do_request(get_monitor_cache, _Args, _Context) ->
    Info = get_cache_info(),
    {200, #{data => Info, code => 200}}.

get_nodes_info(all) ->
    Nodes = emqx_mgmt:list_nodes(),
    [Info ||{_, Info} <- Nodes];
get_nodes_info(Node) ->
    emqx_mgmt:node_info(Node).

get_brokers(all) ->
    Nodes = emqx_mgmt:list_brokers(),
    [Info ||{_, Info} <- Nodes];
get_brokers(Node) ->
    emqx_mgmt:broker_info(Node).

get_stats(all) ->
    Nodes = emqx_mgmt:get_stats(),
    [#{ node => Node, data => Info } ||{Node, Info} <- Nodes];
get_stats(Node) ->
    emqx_mgmt:get_stats(Node).

get_cache_info() ->
    #{

    }.

get_node(#{ <<"node">> := undefined }) -> all;
get_node(#{ <<"node">> := Node }) -> binary_to_atom(Node).

create_zip(FileName, Fs) ->
    case zip:create(FileName, Fs, [memory]) of
        {ok, {FileName, Bin}} ->
            {ok, Bin};
        {error, Reason} ->
            {error, Reason}
    end.
