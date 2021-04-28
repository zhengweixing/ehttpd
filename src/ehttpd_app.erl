-module(ehttpd_app).

-behaviour(application).
-include("ehttpd.hrl").

-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).
-export([check_mod/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    {ok, Sup} = ehttpd_sup:start_link(),
    ok = ehttpd_cache:start(),
    {ok, _} = ehttpd_server:start(?WEBSERVER, ?APP),
    {ok, Sup}.

stop(_State) ->
    ok.


%% 检查模块是否有swagger和route模块
check_mod(ServerName) ->
    S = application:get_env(?APP, include_apps, "*"),
    Apps =
        case S == "*" orelse re:run(S, "([^\\,]+)", [global, {capture, all_but_first, binary}]) of
            true -> "*";
            nomatch -> [];
            {match, List} -> lists:concat(List)
        end,
    Fun =
        fun({App, _Vsn, Mod}, {Handlers, Routers}) ->
            case check_mod(Apps, App, Mod) of
                false ->
                    {Handlers, Routers};
                true ->
                    Handlers1 =
                        case check_attributes(ServerName, Mod, ehttpd_rest) of
                            false -> Handlers;
                            true -> [Mod | Handlers]
                        end,
                    Routers1 =
                        case check_attributes(ServerName, Mod, ehttpd_router) of
                            true -> [Mod | Routers];
                            false -> Routers
                        end,
                    {Handlers1, Routers1}
            end
        end,
    check_module(Fun, {[], []}).

check_module(Check, Acc0) ->
    Fun =
        fun({App, _Desc, Vsn}, Acc) ->
            Dir = lists:concat(["lib/", App, "-", Vsn, "/ebin"]),
            case file:list_dir(Dir) of
                {ok, FS} ->
                    lists:foldl(
                        fun(FileName, Acc1) ->
                            case filename:extension(FileName) == ".beam" of
                                true ->
                                    Mod = list_to_atom(filename:basename(FileName, ".beam")),
                                    Check({App, Vsn, Mod}, Acc1);
                                false ->
                                    Acc1
                            end
                        end, Acc, FS);
                _ ->
                    Acc
            end
        end,
    lists:foldl(Fun, Acc0, application:loaded_applications()).

check_mod(IncApps, App, Mod) ->
    case IncApps == "*" orelse lists:member(atom_to_binary(App, utf8), IncApps) of
        false ->
            false;
        true ->
            not (code:is_loaded(Mod) == false)
    end.

check_attributes(ServerName, Mod, ehttpd_router) ->
    Attributes = [Name||{ehttpd_router, [Name]} <- Mod:module_info(attributes)],
    lists:member(ServerName, Attributes);
check_attributes(ServerName, Mod, ehttpd_rest) ->
    Attributes = [Name||{ehttpd_rest, [Name]} <- Mod:module_info(attributes)],
    lists:member(ServerName, Attributes).
