-module(user_controller).
-behavior(ehttpd_rest).

-ehttpd_rest(default).

%% API
-export([swagger/1, handle/4]).

swagger(default) ->
    [
        ehttpd_server:bind(<<"/swagger_user.json">>, ?MODULE, [], priv)
    ].

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map(), Req :: ehttpd_req:req()}.

handle(OperationID, Args, Context, Req) ->
    do_request(OperationID, Args, Context, Req).

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

do_request(post_logout, Args, Context, Req) ->
    do_request(get_logout, Args, Context, Req);
do_request(post_login, #{<<"username">> := UserName, <<"password">> := Password}, _Context, _Req) ->
    case ehttpd_hook:run('user.login', [UserName, Password], #{}) of
        {ok, Result} ->
            {200, Result};
        {error, Result} ->
            {500, Result}
    end;

do_request(get_getrouters, _Args, #{user := UserInfo}, _Req) ->
    case ehttpd_hook:run('user.get_routers', [UserInfo], #{}) of
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
