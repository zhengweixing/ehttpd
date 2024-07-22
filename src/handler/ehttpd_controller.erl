-module(ehttpd_controller).
-behavior(ehttpd_rest).

-ehttpd_rest(default).

%% API
-export([swagger/1, handle/4]).

swagger(default) ->
    [
        ehttpd_server:bind(<<"/swagger_ehttpd.json">>, ?MODULE, [], priv)
    ].

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map(), Req :: ehttpd_req:req()}.

handle(OperationID, Args, Context, Req) ->
    do_request(OperationID, Args, Context, Req).


do_request(post_generate_api, #{<<"mod">> := Mod} = Args, _Context, _Req) ->
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

do_request(post_upload, #{<<"file">> := FileInfo}, Context, _Req) ->
    {ok, FileInfo1} = ehttpd_hook:run('file.upload', [Context], FileInfo),
    {200, FileInfo1};

do_request(get_permission, #{}, _Context, _Req) ->
    {ok, List} = ehttpd_cache:match({{'$1', permission}, {'$2', '$3', '$4'}}),
    Data = [#{
        name => Rule,
        path => Path,
        summary => Summary,
        desc => Desc
    } || [Rule, Path, Summary, Desc] <- List],
    {200, #{data => Data, code => 200}};


do_request(post_register, Args, Context, _Req) ->
    case ehttpd_hook:run('user.register', [Args, Context], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;

do_request(post_logout, Args, Context, _Req) ->
    case ehttpd_hook:run('user.logout', [Args, Context], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;

do_request(get_login, Args, Context, Req) ->
    do_request(post_login, Args, Context, Req);
do_request(post_login, #{<<"username">> := UserName, <<"password">> := Password}, _Context, Req) ->
    Log = ehttpd_utils:get_log(Req),
    case ehttpd_hook:run('user.login', [UserName, Password, Log], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;
do_request(post_login, #{<<"wechatSession">> := WechatSession}, _Context, Req) ->
    Log = ehttpd_utils:get_log(Req),
    case ehttpd_hook:run('user.login_by_wechat', [WechatSession, Log], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;

do_request(get_getrouters, _Args, #{user := UserInfo}, Req) ->
    case ehttpd_hook:run('user.get_routers', [UserInfo, Req], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;

do_request(get_getinfo, _Args, Context, _Req) ->
    case ehttpd_hook:run('user.get_info', [Context], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;

do_request(get_captchaimage, _Args, _Context, _Req) ->
    {200, #{ captchaEnabled => false, img => <<>> }}.


create_zip(FileName, Fs) ->
    case zip:create(FileName, Fs, [memory]) of
        {ok, {FileName, Bin}} ->
            {ok, Bin};
        {error, Reason} ->
            {error, Reason}
    end.
