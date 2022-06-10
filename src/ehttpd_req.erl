-module(ehttpd_req).
-author("kenneth").

%% API
-export([get_value/3, get_qs/2, get_qs/3, qs/1, parse_qs/1, parse_qs/2, header/2, header/3, headers/1, headers/2, path/1, host/1, binding/2, method/1, read_body/1, read_body/2, set_cookie/4]).
-export([to_lower/1, to_lower/2]).
-export([reply/2,reply/3,reply/4]).


-type req() :: map().
-type http_status() :: cowboy:http_status().
-export_type([req/0, http_status/0]).



get_value(<<"query">>, Name, Req) ->
    {get_qs(Name, Req), Req};

get_value(<<"header">>, Name, Req) ->
    {ehttpd_req:header(Name, Req), Req};

get_value(<<"path">>, Name, Req) ->
    {ehttpd_req:binding(Name, Req), Req};

get_value(<<"cookie">>, Name, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    {proplists:get_value(Name, Cookies), Req};


get_value(<<"body">>, Name, Req0) ->
    {ok, Body, Req} = read_body(Req0),
    Params = jiffy:decode(Body, [return_maps]),
    {maps:get(Name, Params, undefined), Req}.

method(Req) ->
    cowboy_req:method(Req).


-spec get_qs(binary(), req()) -> binary() | undefined.
get_qs(Name, Req) ->
    QS = parse_qs(Req),
    proplists:get_value(Name, QS).

get_qs(Name, Req, Default) ->
    QS = parse_qs(Req),
    proplists:get_value(Name, QS, Default).

parse_qs(Req) ->
    cowboy_req:parse_qs(Req).

parse_qs(Req, Opts) ->
    QS = parse_qs(Req),
    case proplists:get_value(return, Opts) of
        map -> maps:from_list(QS);
        _ -> QS
    end.

qs(Req) ->
    cowboy_req:qs(Req).


-spec header(binary(), req()) -> binary() | undefined.

header(Name, Req, Default) ->
    case header(Name, Req) of
        undefined -> Default;
        Value -> Value
    end.

header(Name0, Req) ->
    Name = to_lower(Name0),
    cowboy_req:header(Name, Req).

headers(Req) ->
    cowboy_req:headers(Req).
headers(Req, Opt) ->
    case proplists:get_value(return, Opt) of
        list ->
            maps:to_list(headers(Req));
        map ->
            headers(Req)
    end.


path(Req) ->
    cowboy_req:path(Req).

host(Req) ->
    cowboy_req:host(Req).

-spec binding(atom(), req()) -> any() | undefined.
binding(Name0, Req0) ->
    Name = binary_to_atom(Name0, utf8),
    cowboy_req:binding(Name, Req0).

-spec read_body(Req :: req()) -> {ok, binary(), req()}.
read_body(Req0) ->
    case maps:get(has_read_body, Req0, false) of
        false ->
            read_body(Req0, fun filter_body/1);
        true ->
            {ok, maps:get(body, Req0, <<>>), Req0}
    end.

filter_body({more, Body1, Req}) ->
    Body0 = maps:get(body, Req, <<>>),
    Body = <<Body0/binary, Body1/binary>>,
    read_body(Req#{body => Body}, fun filter_body/1);
filter_body({ok, Body1, Req}) ->
    Body0 = maps:get(body, Req, <<>>),
    Body = <<Body0/binary, Body1/binary>>,
    case cowboy_req:header(<<"content-type">>, Req) of
        Type when Type == <<"application/x-www-form-urlencoded">>; Type == <<"application/x-www-urlencoded">> ->
            Body2 = jiffy:encode(cow_qs:parse_qs(Body)),
            {ok, Body2, Req#{body =>Body2 }};
        _ ->
            {ok, Body, Req#{body => Body}}
    end.


read_body(Req0, Fun) ->
    Result = cowboy_req:read_body(Req0),
    Fun(Result).

set_cookie(Name, Value, Req, Opts) when is_binary(Name) ->
    set_cookie(binary_to_list(Name), Value, Req, Opts);
set_cookie(Name, Value, Req, Opts) when is_binary(Value) ->
    set_cookie(Name, binary_to_list(Value), Req, Opts);
set_cookie(Name, Value, Req, Opts) ->
    %logger:info("Set-cookie ~p:~p~n", [Name, Value]),
    cowboy_req:set_resp_cookie(Name, Value, Req, Opts).


reply(Status, Req) ->
    cowboy_req:reply(Status, Req).
reply(Status, Headers, Body, Req) ->
    cowboy_req:reply(Status, Headers, Body, Req).
reply(Status, Headers, Req) ->
    cowboy_req:reply(Status, Headers, Req).


to_lower(V, Opts) ->
    V1 = to_lower(V),
    case proplists:get_value(return, Opts) of
        binary ->
            case is_list(V1) of
                true -> list_to_binary(V1);
                false -> V1
            end;
        list ->
            case is_binary(V1) of
                true -> binary_to_list(V1);
                false -> V1
            end;
        atom ->
            case V1 of
                _ when is_binary(V1) -> binary_to_atom(V1);
                _ when is_list(V1) -> list_to_atom(V1)
            end
    end.


to_lower(V) when is_list(V) ->
    string:to_lower(V);
to_lower(V) when is_binary(V) ->
    list_to_binary(string:to_lower(binary_to_list(V))).

