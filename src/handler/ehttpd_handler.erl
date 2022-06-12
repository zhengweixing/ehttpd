-module(ehttpd_handler).
-behavior(ehttpd_rest).
-ehttpd_rest(ehttpd).

%% API
-export([swagger/1, handle/4]).

swagger(ehttpd) ->
    [
        ehttpd_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
    ].


-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map(), Req :: ehttpd_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case do_request(OperationID, Args, Context, Req) of
        {error, Reason} ->
            Err = list_to_binary(io_lib:format("~p", [Reason])),
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            {200, Headers, #{}, Req};
        {ok, Res} ->
            {200, Headers, Res, Req};
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%% OperationId:generate_code
%% POST /iotapi/generate_api
do_request(post_generate_api, #{<<"name">> := Name, <<"mod">> := Mod} = Args, _Context, _Req) ->
    Deps = [compiler, syntax_tools, erlydtl],
    [application:ensure_all_started(App)|| App <- Deps],
    SWSchema = maps:without([<<"mod">>], Args),
    case ehttpd_swagger:compile(Name, binary_to_atom(Mod), SWSchema) of
        {ok, Module, Src} when is_binary(Src) ->
            FileName = "ehttpd_plugin.zip",
            SrcPath = lists:concat(["handler/", Module, ".erl"]),
            SchemaPath = binary_to_list(<<"swagger/swagger_", Mod/binary, ".json">>),
            case create_zip(FileName, [
                {"ehttpd_plugin/priv/" ++ SchemaPath, jiffy:encode(SWSchema)},
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
        Err ->
            Err
    end;

%% OperationId:post_upload
%% POST /iotapi/upload
do_request(post_upload, #{<<"file">> := FileInfo}, Context, _Req) ->
    case ehttpd_server:run_hook('http.upload', [Context], FileInfo) of
        {error, Reason} ->
            {error, Reason};
        {ok, FileInfo1} ->
            {200, FileInfo1}
    end.


create_zip(FileName, Fs) ->
    case zip:create(FileName, Fs, [memory]) of
        {ok, {FileName, Bin}} ->
            {ok, Bin};
        {error, Reason} ->
            {error, Reason}
    end.
