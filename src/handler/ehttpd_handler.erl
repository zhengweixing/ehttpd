%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, kenneth
%%% @doc
%%% API 处理模块 产生时间: Tue, 21 May 2019 16:19:01 +0800
%%% @end
%%%-------------------------------------------------------------------
-module(ehttpd_handler).
-behavior(ehttpd_rest).
-ehttpd_rest(ehttpd).

%% API
-export([swagger_system/0]).
-export([handle/4]).

%% API描述
%% 支持二种方式导入
%% 示例:
%% 1. Metadata为map表示的JSON,
%%    ehttpd_server:bind(<<"/system">>, ?MODULE, [], Metadata)
%% 2. 从模块的priv/swagger/下导入
%%    ehttpd_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
swagger_system() ->
    [
        ehttpd_server:bind(<<"/swagger_system.json">>, ?MODULE, [], priv)
    ].


%%%===================================================================
%%% 请求处理
%%%  如果登录, Context 内有 <<"user">>, version
%%%===================================================================

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map(), Req :: ehttpd_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case catch do_request(OperationID, Args, Context, Req) of
        {ErrType, Reason} when ErrType == 'EXIT'; ErrType == error ->
            logger:info("do request: ~p, ~p, ~p~n", [OperationID, Args, Reason]),
            Err = case is_binary(Reason) of
                      true -> Reason;
                      false -> list_to_binary(io_lib:format("~p", [Reason]))
                  end,
            {500, Headers, #{<<"error">> => Err}};
        ok ->
            % logger:debug("do request: ~p, ~p ->ok ~n", [OperationID, Args]),
            {200, Headers, #{}, Req};
        {ok, Res} ->
            % logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {200, Headers, Res, Req};
        {Status, Res} ->
            % logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            % logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            % logger:debug("do request: ~p, ~p ->~p~n", [OperationID, Args, Res]),
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

%% System 概要: 根据swagger产生代码 描述:根据swagger产生代码
%% OperationId:generate_code
%% 请求:POST /iotapi/generate_api
do_request(post_generate_api, #{<<"mod">> := Mod} = Args, _Context, _Req) ->
    SWSchema = maps:without([<<"mod">>], Args),
    case ehttpd_swagger:compile(binary_to_atom(Mod), SWSchema) of
        {ok, Module, Src} when is_binary(Src) ->
            FileName = "iot_plugin.zip",
            SrcPath = lists:concat(["handler/", Module, ".erl"]),
            SchemaPath = binary_to_list(<<"swagger/swagger_", Mod/binary, ".json">>),
            case create_zip(FileName, [
                {"iot_plugin/priv/" ++ SchemaPath, jsx:encode(SWSchema)},
                {"iot_plugin/src/" ++ SrcPath, Src}
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

%% System 概要: 根据Protobuf产生API代码 描述:根据Protobuf产生API代码
%% 请求:POST /iotapi/generate_grpc
do_request(post_generate_grpc, #{ <<"mod">> := Mod, <<"generate">> := Generate,  <<"protobuf">> := Protobuf }, _Context, _Req) ->
    Out = binary_to_list(<<Mod/binary, "/">>),
    filelib:ensure_dir(Out),
    Path = Out ++ binary_to_list(Mod) ++ ".proto",
    file:write_file(Path, Protobuf),
    ModPath = binary_to_list(<<Mod/binary, "_", Generate/binary, ".erl">>),
    case grpc:compile(Path, [{generate, binary_to_atom(Generate)}]) of
        ok ->
            {ok, Bin} = file:read_file(ModPath),
            file:write_file(Out ++ ModPath, Bin),
            file:delete(ModPath),
            FileName = binary_to_list(<<Mod/binary, ".zip">>),
            Result = create_zip(FileName, [Out]),
            file:del_dir_r(Out),
            case Result of
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

%% System 概要: 服务器文件上传 描述:文件上传到服务器
%% OperationId:post_upload
%% 请求:POST /iotapi/upload
do_request(post_upload, #{<<"file">> := FileInfo}, Context, _Req) ->
    case ehttpd_server:run_hook('http.upload', [Context], FileInfo) of
        {error, Reason} ->
            {error, Reason};
        {ok, FileInfo1} ->
            {200, FileInfo1}
    end;

%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

%%%===================================================================
%%% 内部函数 Version:API版本
%%%===================================================================

create_zip(FileName, Fs) ->
    case zip:create(FileName, Fs, [memory]) of
        {ok, {FileName, Bin}} ->
            {ok, Bin};
        {error, Reason} ->
            {error, Reason}
    end.
