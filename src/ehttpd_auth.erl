%%%-------------------------------------------------------------------
%%% @author zwx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 对于basic auth鉴权 调用add_hook将勾子挂进去
%%% 对于其它方式鉴权，调用 put_session
%%% @end
%%% Created : 10. 五月 2019 3:49
%%%-------------------------------------------------------------------
-module(ehttpd_auth).
-author("zwx").

%% API
-export([pre_check/4]).
-export([check_auth/3]).
-export([put_session/2]).
-export([put_session/3]).
-export([get_session/1]).
-export([ttl/0]).


%% session 期限
ttl() ->
    application:get_env(ehttpd, session_expiration, 1800).

%%%===================================================================
%%% default check auth Callback
%%%===================================================================

-spec check_auth(OperationID :: atom(), Args :: map(), Req :: ehttpd_req:req()) ->
    {true, Context :: #{binary() => any()}, Req :: ehttpd_req:req()} |
    {false, Result :: #{binary() => any()}, Req :: ehttpd_req:req()} |
    {switch_handler, Mod :: module(), Req :: ehttpd_req:req()}.

check_auth(OperationID, #{<<"username">> := UserName, <<"password">> := Password}, Req) ->
    is_authorized(OperationID, {UserName, Password}, Req);
check_auth(OperationID, #{<<"apiKey">> := ApiKey}, Req) ->
    is_authorized(OperationID, ApiKey, Req).


%%%===================================================================
%%% pre check args
%%%===================================================================

-spec pre_check(OperationID :: atom(), LogicHandler :: atom(), AuthList :: list() | atom(), Req :: ehttpd_req:req()) ->
    {ok, Args :: #{binary() => any()}, Req :: ehttpd_req:req()} |
    {error, Err :: binary(), Req :: ehttpd_req:req()}.

pre_check(_OperationID, _LogicHandler, [], Req) ->
    {error, <<"unauthorized">>, Req};

pre_check(OperationID, LogicHandler, [{<<"BasicAuth">> = Key, _Map} | AuthList], Req) ->
    case ehttpd_req:get_value(<<"header">>, <<"authorization">>, Req) of
        {undefined, Req1} ->
            pre_check(OperationID, LogicHandler, AuthList, Req1);
        {<<"Basic ", Bin/binary>>, Req1} ->
            case re:split(base64:decode(Bin), <<":">>) of
                [UserName, Password] when byte_size(UserName) > 0, byte_size(Password) > 0 ->
                    Args = #{
                        <<"type">> => Key,
                        <<"username">> => UserName,
                        <<"password">> => Password
                    },
                    {ok, Args, Req1};
                _ ->
                    pre_check(OperationID, LogicHandler, AuthList, Req1)
            end
    end;

pre_check(OperationID, LogicHandler, [{Key, #{<<"in">> := From, <<"name">> := Name}} | AuthList], Req) ->
    case ehttpd_req:get_value(From, Name, Req) of
        {undefined, Req1} ->
            pre_check(OperationID, LogicHandler, AuthList, Req1);
        {ApiKey, Req1} ->
            Args = #{
                <<"type">> => Key,
                <<"apiKey">> => ApiKey
            },
            {ok, Args, Req1}
    end.


%%%===================================================================
%%% 内部函数
%%%===================================================================

%% Basic auth
is_authorized(OperationID, {UserName, Password}, Req) ->
    Key = erlang:md5(binary_to_list(<<UserName/binary, Password/binary>>)),
    Token = ehttpd_cache:get(Key),
    case is_binary(Token) andalso byte_size(Token) > 0 andalso get_session(Token) =/= undefined of
        true ->
            is_authorized(OperationID, Token, Req);
        false ->
            % login'
            case ehttpd_server:run_hook('http.auth', [UserName, Password], #{  }) of
                {ok, #{ sessionToken := SessionToken }} ->
                    ehttpd_cache:set(Key, SessionToken, ttl()),
                    is_authorized(OperationID, SessionToken, Req);
                _ ->
                    {false, #{<<"code">> => 209, <<"error">> => <<"unauthorized">>}, Req}
            end
    end;

%% Token auth, in body, query, header, cookie
is_authorized(OperationID, Token, Req) ->
    Action = list_to_binary(string:to_upper(atom_to_list(OperationID))),
    case get_session(Token) of
        undefined ->
            {false, #{<<"code">> => 209, <<"error">> => <<"unauthorized">>}, Req};
        #{<<"rules">> := Rules} = UserInfo ->
            %% 检查操作权限
            case lists:member(Action, Rules) of
                false ->
                    {forbidden, #{<<"code">> => 119, <<"error">> => <<Action/binary, " Forbidden">>}, Req};
                true ->
                    {true, #{<<"user">> => UserInfo, <<"sessionToken">> => Token}, Req}
            end
    end.



put_session(#{<<"sessionToken">> := SessionToken} = UserInfo, TTL) ->
    put_session(SessionToken, maps:remove(<<"sessionToken">>, UserInfo), TTL).

put_session(SessionToken, UserInfo, TTL) ->
    Key = <<"session#", SessionToken/binary>>,
    ehttpd_cache:set(Key, jsx:encode(UserInfo), TTL),
    ok.

get_session(SessionToken) ->
    Key = <<"session#", SessionToken/binary>>,
    case ehttpd_cache:get(Key) of
        <<>> ->
            undefined;
        User when is_binary(User) ->
            jsx:decode(User, [{labels, binary}, return_maps])
    end.
