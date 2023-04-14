-module(ehttpd_utils).
-author("weixingzheng").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([md5/1, check_module/1, filename_join/2, get_log/1]).

-define(IGNORE_APPS, [
    kernel, sasl, crypto, public_key, asn1, syntax_tools,
    ssl, os_mon, inets, compiler, runtime_tools, redbug,
    xmerl, sasl, stdlib, appmon, eldap, erts, mnesia, inets,
    goldrush, gproc, snmp, otp_mibs, ssh, hipe, common_test,
    observer, webtool, xmerl, tools, debugger, eunit, et,
    wx
]).

md5(V) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(V)])).

check_module(_Name) ->
    Fun =
        fun(Mod, {Handlers, Routers}) ->
            Handlers1 =
                case check_attributes(Mod, ehttpd_rest) of
                    false -> Handlers;
                    true -> [Mod | Handlers]
                end,
            Routers1 =
                case check_attributes(Mod, ehttpd_router) of
                    true -> [Mod | Routers];
                    false -> Routers
                end,
            {Handlers1, Routers1}
        end,
    filter_module(Fun).

filter_module(Check) ->
    Fun =
        fun(Dir, Acc) ->
            Re = ".+/(.+?)(-.+)?/ebin",
            case re:run(Dir, Re, [{capture, all_but_first, list}]) of
                {match, [App | _]} ->
                    case lists:member(list_to_atom(App), ?IGNORE_APPS) of
                        true ->
                            Acc;
                        false ->
                            lists:foldl(
                                fun(Mod, Acc1) ->
                                    Check(Mod, Acc1)
                                end, Acc, get_module(Dir))
                    end;
                _ ->
                    Acc
            end
        end,
    lists:foldl(Fun, {[], []}, code:get_path()).

check_attributes(Mod, ehttpd_router) ->
    Attributes = Mod:module_info(attributes),
    lists:member(ehttpd_router, proplists:get_keys(Attributes)) andalso
    erlang:function_exported(Mod, route, 2);
check_attributes(Mod, ehttpd_rest) ->
    Attributes = Mod:module_info(attributes),
    lists:member(ehttpd_rest, proplists:get_keys(Attributes)) andalso
    erlang:function_exported(Mod, swagger, 1).

get_module(Dir) ->
    case file:list_dir(Dir) of
        {ok, FS} ->
            lists:foldl(
                fun(FileName, Acc) ->
                    case filename:extension(FileName) == ".beam" of
                        true ->
                            Mod = filename:basename(FileName, ".beam"),
                            [list_to_atom(Mod) | Acc];
                        false ->
                            Acc
                    end
                end, [], FS);
        {error, _Reason} ->
            []
    end.

filename_join(Dir, <<"/", Path/binary>>) ->
    filename_join(Dir, Path);
filename_join(Dir, Path) ->
    filename:join([Dir, Path]).


get_log(Req) ->
    Path = cowboy_req:path(Req),
    UserAgent = cowboy_req:header(<<"user-agent">>, Req),
    Peer = get_peer(Req),
    Method = cowboy_req:method(Req),
    #{
        <<"Method">> => Method,
        <<"UserAgent">> => UserAgent,
        <<"OS">> => get_os(Req, UserAgent),
        <<"Path">> => Path,
        <<"Peer">> => Peer
    }.

get_os(Req, UserAgent) ->
    case cowboy_req:header(<<"sec-ch-ua-platform">>, Req) of
        undefined ->
            case re:run(UserAgent, <<"Mac|Windows|IOS|Android|HarmonyOS|Linux">>, [{capture,all,binary}]) of
                {match, [Os|_]} -> Os;
                nomatch -> undefined
            end;
        Os ->
            re:replace(Os, <<"\"">>, <<>>, [global, {return, binary}])
    end.

get_peer(Req) ->
    case cowboy_req:header(<<"X-Real-IP">>, Req) of
        undefined ->
            {Addr, Port} = cowboy_req:peer(Req),
            list_to_binary(inet:ntoa(Addr) ++ ":" ++ integer_to_list(Port));
        Peer ->
            Peer
    end.

