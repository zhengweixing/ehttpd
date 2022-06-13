-module(user_controller).
-behavior(ehttpd_rest).

-ehttpd_rest(ehttpd).

%% API
-export([swagger/1, handle/4]).

swagger(ehttpd) ->
    [
        ehttpd_server:bind(<<"/swagger_user.json">>, ?MODULE, [], priv)
    ].

-spec handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Status :: ehttpd_req:http_status(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map()} |
    {Status :: ehttpd_req:http_status(), Headers :: map(), Body :: map(), Req :: ehttpd_req:req()}.

handle(OperationID, Args, Context, Req) ->
    Headers = #{},
    case do_request(OperationID, Args, Context, Req) of
        {Status, Res} ->
            {Status, Headers, Res, Req};
        {Status, NewHeaders, Res} ->
            {Status, maps:merge(Headers, NewHeaders), Res, Req};
        {Status, NewHeaders, Res, NewReq} ->
            {Status, maps:merge(Headers, NewHeaders), Res, NewReq}
    end.


do_request(get_user_id, _Args, Context, _Req) ->
    Response = maps:get(check_response, Context, #{}),
    % @todo 自己实现函数
    {200, Response};

do_request(post_user, _Args, Context, _Req) ->
    Response = maps:get(check_response, Context, #{}),
    % @todo 自己实现函数
    {200, Response};

do_request(post_passwordreset, _Args, Context, _Req) ->
    Response = maps:get(check_response, Context, #{}),
    % @todo 自己实现函数
    {200, Response};

do_request(post_logout, _Args, Context, _Req) ->
    Response = maps:get(check_response, Context, #{}),
    % @todo 自己实现函数
    {200, Response};

do_request(get_login, _Args, Context, _Req) ->
    Response = maps:get(check_response, Context, #{}),
    % @todo 自己实现函数
    {200, Response};

do_request(_OperationId, _Args, _Context, _Req) ->
    {403, #{ error => forbidden }}.
