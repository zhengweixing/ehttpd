%%%-------------------------------------------------------------------
%%% @author zwx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% API回调模块
%%% @end
%%% Created : 28. 四月 2019 1:53
%%%-------------------------------------------------------------------
-module(ehttpd_mysql_handler).
-author("zwx").
-behavior(ehttpd_rest).
-ehttpd_rest(ehttpd).
-define(RE_OPTIONS, [global, {return, binary}]).

%% API
-export([swagger_mysql/0, handle/4]).


%%%===================================================================
%%% swagger Callback
%%%===================================================================
swagger_mysql() ->
    S = application:get_env(ehttpd, swagger_tables, "*"),
    Tables =
        case S == "*" orelse re:run(S, "([^\\,]+)", [global, {capture, all_but_first, binary}]) of
            true -> "*";
            nomatch -> [];
            {match, List} -> lists:concat(List)
        end,
    case mysql_adapter:get_schemas() of
        {ok, #{<<"results">> := Schemas}} ->
            Fun =
                fun(#{<<"className">> := ClassName} = Schema, Acc) ->
                    case Tables == "*" orelse lists:member(ClassName, Tables) of
                        true ->
                            case catch get_paths(Schema, Acc) of
                                {'EXIT', Reason} ->
                                    logger:warning("~p,~p~n", [Schema, Reason]),
                                    Acc;
                                NewAcc ->
                                    NewAcc
                            end;
                        false ->
                            Acc
                    end
                end,
            lists:foldl(Fun, #{}, Schemas);
        {error, Reason} ->
            {error, Reason}
    end.

%%%===================================================================
%%% 请求回调
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


%%  服务器不支持的API接口
do_request(_OperationId, _Args, _Context, _Req) ->
    {error, <<"Not Allowed.">>}.

transform_class(#{<<"className">> := ClassName, <<"fields">> := Fields}, ComSchemas) ->
    Fun =
        fun(Key, Map, Acc) ->
            Acc#{Key => Map#{<<"field">> => Key}}
        end,
    Properties = maps:fold(Fun, #{}, Fields),
    DelCols = [],
    Props = maps:without(DelCols, Properties),
    maps:merge(#{
        ClassName => #{
            <<"type">> => <<"object">>,
            <<"properties">> => Props
        }
    }, ComSchemas).


get_paths(#{<<"className">> := ClassName} = Schema, Acc) ->
    Definitions = maps:get(<<"definitions">>, Acc, #{}),
    Paths = maps:get(<<"paths">>, Acc, #{}),
    Tags = maps:get(<<"tags">>, Acc, []),
    CSchema = get_path(Tags, ClassName),
    CDefinitions = maps:get(<<"definitions">>, CSchema, #{}),
    CPaths = maps:get(<<"paths">>, CSchema, #{}),
    CTags = maps:get(<<"tags">>, CSchema, []),
    Acc#{
        <<"definitions">> => transform_class(maps:merge(CDefinitions, Schema), Definitions),
        <<"paths">> => maps:merge(Paths, CPaths),
        <<"tags">> => lists:foldl(
            fun(#{<<"name">> := TagName} = Tag0, Acc1) ->
                NewTags = lists:filtermap(
                    fun(#{<<"name">> := OldTagName}) ->
                        OldTagName =/= TagName
                    end, Acc1),
                [Tag0 | NewTags]
            end, Tags, CTags)
    }.


get_path(Tags, ClassName) ->
    {ok, Bin} = ehttpd_swagger:load_schema(?MODULE, "swagger_mysql_object.json", []),
    Data = re:replace(Bin, "\\{\\{className\\}\\}", ClassName, ?RE_OPTIONS),
    CTags = lists:filtermap(fun(#{<<"name">> := Name}) -> Name == ClassName end, Tags),
    Desc =
        case CTags of
            [] -> ClassName;
            [#{<<"description">> := Description} | _] -> Description
        end,
    Data1 = re:replace(Data, "\\{\\{tag\\}\\}", Desc, ?RE_OPTIONS),
    jsx:decode(Data1, [{labels, binary}, return_maps]).
