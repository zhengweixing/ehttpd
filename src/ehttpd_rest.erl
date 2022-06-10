-module(ehttpd_rest).
-include("ehttpd.hrl").
-author("kenneth").

%% basic handler

%% Cowboy REST callbacks
-export([allowed_methods/2]).
-export([init/2]).
-export([allow_missing_post/2]).
-export([content_types_accepted/2]).
-export([charsets_provided/2]).
-export([content_types_provided/2]).
-export([delete_resource/2]).
-export([is_authorized/2]).
-export([known_content_type/2]).
-export([malformed_request/2]).
-export([valid_content_headers/2]).
-export([valid_entity_length/2]).

%% Handlers
-export([handle_request/2]).
-export([handle_multipart/2]).
-export([call/3]).

-record(state, {
    operationid :: atom(),
    logic_handler :: atom(),
    context = #{} :: #{}
}).

-type state() :: state().
-type response() :: {Result :: stop | binary(), ehttpd_req:req(), state()}.

%% Common handler callbacks.
-callback init(Req :: ehttpd_req:req(), Config :: map()) ->
    {Mod :: module(), NewReq :: ehttpd_req:req(), Context :: map()}.
-optional_callbacks([init/2]).

-callback check_auth(Args :: map(), Req :: ehttpd_req:req()) ->
    {true, Context :: #{binary() => any()}, Req :: ehttpd_req:req()} |
    {false, Result :: #{binary() => any()}, Req :: ehttpd_req:req()} |
    {switch_handler, Mod :: module(), Req :: ehttpd_req:req()}.
-optional_callbacks([check_auth/2]).

-callback handle(OperationID :: atom(), Args :: map(), Context :: map(), Req :: ehttpd_req:req()) ->
    {Code :: ehttpd_req:http_status(), Header :: ehttpd_req:http_headers(), Body :: map()}.


-spec init(Req :: ehttpd_req:req(), Opts :: map()) ->
    {cowboy_rest, Req :: ehttpd_req:req(), Opts :: state()}.
init(Req, #{logic_handler := LogicHandler} = Map) ->
    State = #state{
        logic_handler = LogicHandler
    },
    Method = ehttpd_req:method(Req),
    case Method of
        <<"OPTIONS">> ->
            default_init(#{ operationid => options }, State, Req);
        _ ->
            case call(LogicHandler, init, [Req, Map]) of
                {no_call, Req1} ->
                    Index = maps:get(Method, Map),
                    {ok, {_, Config}} = ehttpd_router:get_state(Index),
                    default_init(Config, State, Req1);
                no_call ->
                    Index = maps:get(Method, Map),
                    {ok, {_, Config}} = ehttpd_router:get_state(Index),
                    default_init(Config, State, Req);
                {?MODULE, Req1, NewConfig} ->
                    default_init(NewConfig, State, Req1)
            end
    end.


default_init(Config, State, Req) ->
    OperationId = maps:get(operationid, Config, not_allowed),
    {cowboy_rest, Req, State#state{
        operationid = OperationId,
        context = maps:without([operationid], Config)
    }}.


-spec allowed_methods(Req :: ehttpd_req:req(), State :: state()) ->
    {Value :: [binary()], Req :: ehttpd_req:req(), State :: state()}.
allowed_methods(Req, State = #state{operationid = OperationId}) ->
    Method = ehttpd_req:method(Req),
    case Method == <<"OPTIONS">> orelse OperationId =/= not_allowed of
        true ->
            {[Method], Req, State};
        false ->
            {[], Req, State}
    end.


-spec is_authorized(Req :: ehttpd_req:req(), State :: state()) ->
    {
        Value :: true | {false, AuthHeader :: iodata()},
        Req :: ehttpd_req:req(),
        State :: state()
    }.
is_authorized(Req0, #state{ operationid = options } = State) ->
    case ?ACCESS_CONTROL_ALLOW_HEADERS of
        <<"false">> ->
            ehttpd_req:reply(403, ?HEADER, <<>>, Req0);
        Header ->
            do_response(200, ?HEADER#{
                <<"access-control-allow-headers">> => Header
            }, #{}, Req0, State)
    end;

is_authorized(Req0, State = #state{
    operationid = OperationID,
    logic_handler = LogicHandler,
    context = Context
}) ->
    AuthList = maps:get(authorize, Context, []),
    CheckResult = ehttpd_auth:pre_check(OperationID, LogicHandler, AuthList, Req0),
    case CheckResult of
        {ok, Args, Req} ->
            case do_authorized(LogicHandler, OperationID, Args, Req) of
                {true, NContext, Req1} ->
                    {true, Req1, State#state{context = maps:merge(Context, NContext)}};
                {forbidden, Err, Req1} when AuthList =/= [] ->
                    do_response(403, #{}, Err, Req1, State);
                {false, Err, Req1} when AuthList =/= [] ->
                    do_response(401, #{}, Err, Req1, State);
                {_, _Err, Req1} ->
                    {true, Req1, State}
            end;
        {error, Err, Req1} when AuthList =/= [] ->
            do_response(401, #{}, #{error => Err}, Req1, State);
        {error, _Err, Req1} ->
            {true, Req1, State}
    end.



-spec content_types_accepted(Req :: ehttpd_req:req(), State :: state()) ->
    {
        Value :: [{binary(), AcceptResource :: atom()}],
        Req :: ehttpd_req:req(),
        State :: state()
    }.
content_types_accepted(Req, #state{context = #{consumes := Consumes}} = State) ->
    Accepts =
        lists:foldl(
            fun
                (<<"multipart/form-data">>, Accepted) -> [{'*', handle_multipart} | Accepted];
                (_, Accepted) -> [{'*', handle_request} | Accepted]
            end, [], Consumes),
    {Accepts, Req, State}.


-spec valid_content_headers(Req :: ehttpd_req:req(), State :: state()) ->
    {Value :: boolean(), Req :: ehttpd_req:req(), State :: state()}.
valid_content_headers(Req, State) ->
    {true, Req, State}.


-spec content_types_provided(Req :: ehttpd_req:req(), State :: state()) ->
    {
        Value :: [{binary(), ProvideResource :: atom()}],
        Req :: ehttpd_req:req(),
        State :: state()
    }.
content_types_provided(Req, #state{context = #{produces := Produces}} = State) ->
    Product =
        lists:foldl(
            fun
                (<<"*">>, Accepted) -> [{'*', handle_request} | Accepted];
                (Consume, Accepted) -> [{Consume, handle_request} | Accepted]
            end, [], Produces),
    {Product, Req, State}.


charsets_provided(Req, State) ->
    {[<<"UTF-8">>], Req, State}.


-spec malformed_request(Req :: ehttpd_req:req(), State :: state()) ->
    {Value :: false, Req :: ehttpd_req:req(), State :: state()}.
malformed_request(Req, State) ->
    {false, Req, State}.


-spec allow_missing_post(Req :: ehttpd_req:req(), State :: state()) ->
    {Value :: false, Req :: ehttpd_req:req(), State :: state()}.
allow_missing_post(Req, State) ->
    {false, Req, State}.


-spec delete_resource(Req :: ehttpd_req:req(), State :: state()) -> response().
delete_resource(Req, State) ->
    handle_request(Req, State).


-spec known_content_type(Req :: ehttpd_req:req(), State :: state()) ->
    {Value :: true, Req :: ehttpd_req:req(), State :: state()}.
known_content_type(Req, State) ->
    {true, Req, State}.


-spec valid_entity_length(Req :: ehttpd_req:req(), State :: state()) ->
    {Value :: true, Req :: ehttpd_req:req(), State :: state()}.
valid_entity_length(Req, State) ->
    %% @TODO check the length
    {true, Req, State}.


-spec handle_multipart(ehttpd_req:req(), state()) -> response().
handle_multipart(Req, State) ->
    case handle_multipart(Req, #{}, State) of
        {ok, Populated, Req1} ->
            case catch do_request(Populated, Req1, State) of
                {'EXIT', Reason} ->
                    Err = list_to_binary(io_lib:format("~p", [Reason])),
                    do_response(500, #{}, #{error => Err}, Req, State);
                {Status, Body} ->
                    do_response(Status, #{}, Body, Req, State);
                {Status, Headers, Body} ->
                    do_response(Status, Headers, Body, Req, State);
                {Status, Headers, Body, NewReq} ->
                    do_response(Status, Headers, Body, NewReq, State)
            end;
        {error, Reason} ->
            Err = list_to_binary(io_lib:format("~p", [Reason])),
            do_response(500, #{}, #{error => Err}, Req, State)
    end.

-spec handle_request(ehttpd_req:req(), state()) -> response().
handle_request(Req, #state{context = Context} = State) ->
    Version = maps:get(version, Context),
    CurVersion = ehttpd_req:get_qs(<<"version">>, Req, Version),
    NewState = State#state{context = Context#{version => CurVersion}},
    try
        {ok, Populated, Req1} = ehttpd_rest_check:check_request(Context, Req),
        case do_request(Populated, Req1, NewState) of
            {Status, Body} ->
                do_response(Status, #{}, Body, Req, State);
            {Status, Headers, Body} ->
                do_response(Status, Headers, Body, Req, State);
            {Status, Headers, Body, NewReq} ->
                do_response(Status, Headers, Body, NewReq, State)
        end
    catch
        throw:{wrong_param, _Name, required, Reason} ->
            do_response(400, #{}, #{error => Reason}, Req, State);
        throw:{wrong_param, Reason, schema, wrong_type} ->
            Err = list_to_binary(io_lib:format("~p, wrong type", [Reason])),
            do_response(500, #{}, #{error => Err}, Req, State);
        Reason ->
            Err = list_to_binary(io_lib:format("~p", [Reason])),
            do_response(500, #{}, #{error => Err}, Req, State)
    end.

do_request(Populated, Req0, State = #state{
    operationid = OperationID,
    logic_handler = LogicHandler,
    context = Context
}) ->
    Args = [OperationID, Populated, Context, Req0],
    Result = call(LogicHandler, handle, Args),
    case Result of
        no_call ->
            Why = list_to_binary(io_lib:format("~p function handle not exported", [LogicHandler])),
            do_response(500, #{}, #{error => Why}, Req0, State);
        _ ->
            Result
    end.


handle_multipart(Req, Acc, State) ->
    case cowboy_req:read_part(Req) of
        undefined ->
            {ok, Acc, Req};
        {ok, Headers, Req1} ->
            handle_multipart(cow_multipart:form_data(Headers), Req1, Acc, State);
        {done, Req1} ->
            {ok, Acc, Req1}
    end.
handle_multipart({data, Name}, Req, Acc, State) ->
    {ok, Data, Req1} = cowboy_req:read_part_body(Req),
    handle_multipart(Req1, Acc#{Name => Data}, State);
handle_multipart({file, Name, Filename, ContentType}, Req, Acc, State) ->
    DocRoot = list_to_binary(ehttpd_server:get_env(ehttpd,docroot)),
    {{Y, M, D}, {H, N, S}} = calendar:local_time(),
    Now = list_to_binary(lists:concat([Y, M, D, H, N, S])),
    Exe = filename:extension(Filename),
    FilePath = <<"upload/", Now/binary, Exe/binary>>,
    Path = filename:join([DocRoot, FilePath]),
    case filelib:ensure_dir(Path) of
        ok ->
            {ok, Bin, Req1} = cowboy_req:read_part_body(Req),
            case file:write_file(Path, Bin, [append]) of
                ok ->
                    handle_multipart(Req1, Acc#{
                        Name => #{
                            <<"path">> => <<"/", FilePath/binary>>,
                            <<"filename">> => Filename,
                            <<"contentType">> => ContentType
                        }}, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.


do_authorized(LogicHandler, OperationID, Args, Req) ->
    case call(LogicHandler, check_auth, [OperationID, Args, Req]) of
        no_call ->
            ehttpd_auth:check_auth(OperationID, Args, Req);
        {true, NContext, Req1} ->
            {true, NContext, Req1};
        {forbidden, Err, Req1} ->
            {forbidden, Err, Req1};
        {false, Err, Req1} ->
            {false, Err, Req1};
        {switch_handler, NLogicHandler, Req1} ->
            call(NLogicHandler, check_auth, [OperationID, Args, Req1])
    end.


do_response(Status, Headers, Body, Req, State) when is_map(Body); is_list(Body) ->
    do_response(Status, Headers, jiffy:encode(Body), Req, State);
do_response(Status, Headers, Body, Req0, State) when is_binary(Body) ->
    NewHeaders = maps:merge(Headers, ?HEADER),
    Req =
        case Status == 500 of
            true ->
                case application:get_env(ehttpd, debug, false) of
                    true ->
                        logger:info("Response 500:~p~n", [Body]),
                        ehttpd_req:reply(Status, NewHeaders, Body, Req0);
                    false ->
                        Res = #{<<"error">> => <<"Server Internal error">>},
                        ehttpd_req:reply(Status, NewHeaders, jiffy:encode(Res), Req0)
                end;
            false ->
                ehttpd_req:reply(Status, NewHeaders, Body, Req0)
        end,
    {stop, Req, State}.


call(Mod, Fun, Args) ->
    case erlang:function_exported(Mod, Fun, length(Args)) of
        true ->
            apply(Mod, Fun, Args);
        false ->
            no_call
    end.
