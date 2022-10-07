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

do_request(get_monitor_cache, _Args, _Context) ->
    Info = get_cache_info(),
    {200, #{data => Info, code => 200}};

do_request(get_monitor_server, _Args, _Context) ->
    Info = get_server_info(),
    {200, #{data => Info, code => 200}}.


get_server_info() ->
    #{
        cpu => #{
            cpuNum => 2,
            free => 97.46,
            sys => 0.51,
            total => 197000,
            used => 1.52,
            wait => 0.51
        },
        jvm => #{
            free => 295.48,
            home => <<"/">>,
            inputArgs => <<>>,
            max => 1008,
            name => <<"Java HotSpot(TM) 64-Bit Server VM">>,
            runTime => <<"2天0小时54分钟"/utf8>>,
            startTime => <<"2022-06-20 08:58:26">>,
            total => 509.5,
            usage => 42.01,
            used => 214.02,
            version => <<"1.8.0_111">>
        },
        mem => #{
            free => 3.88,
            total => 7.56,
            usage => 48.71,
            used => 3.68
        },
        sys => #{
            computerIp => <<"">>,
            computerName => <<"">>,
            osArch => <<"amd64">>,
            osName => <<"Linux">>
        },
        sysFiles => [
            #{
                dirName => <<"/">>,
                free => <<"21.6 GB">>,
                sysTypeName => <<"ext4">>,
                total => <<"39.2 GB">>,
                typeName => <<"/">>,
                usage => 44.95,
                used => <<"17.6 GB">>
            }
        ]
    }.

get_cache_info() ->
    #{

    }.


create_zip(FileName, Fs) ->
    case zip:create(FileName, Fs, [memory]) of
        {ok, {FileName, Bin}} ->
            {ok, Bin};
        {error, Reason} ->
            {error, Reason}
    end.
