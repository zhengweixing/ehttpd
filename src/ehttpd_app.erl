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
    Env = ehttpd_server:get_env(?APP),
    {ok, _} = ehttpd_server:start(?APP, Env),
    {ok, Sup}.

stop(_State) ->
    ok.

check_mod(Name) ->
    Fun =
        fun(Mod, {Handlers, Routers}) ->
            Handlers1 =
                case check_attributes(Name, Mod, ehttpd_rest) of
                    false -> Handlers;
                    true -> [Mod | Handlers]
                end,
            Routers1 =
                case check_attributes(Name, Mod, ehttpd_router) of
                    true -> [Mod | Routers];
                    false -> Routers
                end,
            {Handlers1, Routers1}
        end,
    check_module(Fun, {[], []}).

check_module(Check, Acc0) ->
    Fun =
        fun(Dir, Acc) ->
            case re:run(Dir, ".+/(.+?)(-.+)?/ebin", [{capture,all_but_first,binary}]) of
                nomatch ->
                    Acc;
                {match,[_App|_]} ->
                    case file:list_dir(Dir) of
                        {ok, FS} ->
                            lists:foldl(
                                fun(FileName, Acc1) ->
                                    case filename:extension(FileName) == ".beam" of
                                        true ->
                                            Mod = list_to_atom(filename:basename(FileName, ".beam")),
                                            Check(Mod, Acc1);
                                        false ->
                                            Acc1
                                    end
                                end, Acc, FS);
                        _ ->
                            Acc
                    end
            end
        end,
    lists:foldl(Fun, Acc0, code:get_path()).

check_attributes(ServerName, Mod, ehttpd_router) ->
    Attributes = [Name||{ehttpd_router, [Name]} <- Mod:module_info(attributes)],
    lists:member(ServerName, Attributes);
check_attributes(ServerName, Mod, ehttpd_rest) ->
    Attributes = [Name||{ehttpd_rest, [Name]} <- Mod:module_info(attributes)],
    lists:member(ServerName, Attributes).
