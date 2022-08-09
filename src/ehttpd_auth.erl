-module(ehttpd_auth).

%% API
-export([pre_check/2, check_auth/2, put_session/3, get_session/1, delete_session/1]).

-spec check_auth(Args :: map(), Context :: ehttpd_rest:context()) ->
    anonymous | {true, Token, UserInfo} | {forbidden, Token, UserInfo} when
    Token :: binary(),
    UserInfo :: #{binary() => any()}.
check_auth(#{<<"username">> := UserName, <<"password">> := Password}, Context) ->
    Key = erlang:md5(binary_to_list(<<UserName/binary, Password/binary>>)),
    Token = ehttpd_cache:get_with_ttl(Key),
    case not lists:member(Token, [undefined, expired]) andalso get_session(Token) of
        undefined ->
            case ehttpd_hook:run('user.login', [UserName, Password], #{}) of
                {ok, #{sessionToken := SessionToken}} ->
                    TTL = ehttpd_server:get_env(default, expire, 1800),
                    ehttpd_cache:set_with_ttl(Key, SessionToken, TTL),
                    has_role(SessionToken, Context);
                _ ->
                    anonymous
            end;
        _ ->
            has_role(Token, Context)
    end;
check_auth(#{<<"token">> := Token}, Context) ->
    has_role(Token, Context);
check_auth(_, _Context) ->
    anonymous.


-spec pre_check(AuthList :: list(), Req) ->
    {undefined | Args, Req} when
    Args :: map(),
    Req :: ehttpd_req:req().
pre_check([], Req) ->
    {undefined, Req};
pre_check([{<<"BasicAuth">> = Key, _Map} | AuthList], Req) ->
    case ehttpd_req:get_value(<<"header">>, <<"authorization">>, Req) of
        {undefined, Req1} ->
            pre_check(AuthList, Req1);
        {Authorization, Req1} ->
            case binary:split(Authorization, <<" ">>) of
                [_, Bin] when Bin == <<"undefined">>; Bin == <<>> ->
                    pre_check(AuthList, Req1);
                [<<"Bearer">>, Token] ->
                    {#{
                        <<"type">> => Key,
                        <<"token">> => Token
                    }, Req1};
                [_, Bin] ->
                    case re:split(base64:decode(Bin), <<":">>) of
                        [UserName, Password] ->
                            {#{
                                <<"type">> => Key,
                                <<"username">> => UserName,
                                <<"password">> => Password
                            }, Req};
                        _ ->
                            pre_check(AuthList, Req1)
                    end
            end
    end;
pre_check([{Key, #{<<"in">> := From, <<"name">> := Name}} | AuthList], Req) ->
    case ehttpd_req:get_value(From, Name, Req) of
        {undefined, Req1} ->
            pre_check(AuthList, Req1);
        {Token, Req1} ->
            {#{
                <<"type">> => Key,
                <<"token">> => Token
            }, Req1}
    end.

%% Token in body, query, header, cookie
-spec has_role(Token, Context :: map()) ->
    anonymous | {forbidden, Token, UserInfo} | {true, Token, UserInfo} when
    Token :: binary(),
    UserInfo :: map().
has_role(Token, #{rule := Rule}) ->
    case get_session(Token) of
        undefined ->
            anonymous;
        UserInfo ->
            Rules = maps:get(<<"rules">>, UserInfo, []),
            Permissions = maps:get(<<"permissions">>, UserInfo, []),
            case check_role(Rule, lists:concat([Rules, Permissions])) of
                false ->
                    {forbidden, Token, UserInfo};
                true ->
                    {true, Token, UserInfo}
            end
    end.


%% <<"system:user:edit">>, [<<"*:*:*">>]
-spec check_role(Rule, [Rule]) -> boolean() when
    Rule :: binary().
check_role(_, []) -> false;
check_role(Rule, [<<>> | Permissions]) ->
    check_role(Rule, Permissions);
check_role(Rule, [Rule | _Permissions]) ->
    true;
check_role(Rule, [Permission | Permissions]) ->
    R = binary:split(Rule, <<":">>, [global]),
    P = binary:split(Permission, <<":">>, [global]),
    case check_permission(R, P) of
        true -> true;
        false -> check_role(Rule, Permissions)
    end.

-spec check_permission(list(), list()) -> boolean().
check_permission([], []) -> true;
check_permission(_, []) -> true;
check_permission([], _) -> false;
check_permission([_ | R1], [<<"*">> | R2]) ->
    check_permission(R1, R2);
check_permission([R1 | R2], [R1 | R3]) ->
    check_permission(R2, R3);
check_permission(_, _) ->
    false.


-spec put_session(Token, UserInfo, TTL) -> true when
    TTL :: integer(),
    Token :: binary(),
    UserInfo :: map().
put_session(Token, UserInfo, TTL) ->
    Key = <<"session#", Token/binary>>,
    ehttpd_cache:set_with_ttl(Key, jiffy:encode(UserInfo), TTL).

-spec get_session(Token) -> undefined | UserInfo :: map() when
    Token :: binary().
get_session(Token) ->
    Key = <<"session#", Token/binary>>,
    case ehttpd_cache:get_with_ttl(Key) of
        expired ->
            undefined;
        undefined ->
            undefined;
        User ->
            jiffy:decode(User, [return_maps])
    end.

-spec delete_session(Token :: binary()) -> true.
delete_session(Token) ->
    Key = <<"session#", Token/binary>>,
    ehttpd_cache:delete_with_ttl(Key).
