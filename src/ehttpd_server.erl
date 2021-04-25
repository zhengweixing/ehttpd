%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%% 通用型HTTP服务器
%%% @end
%%% Created : 24. 四月 2019 16:43
%%%-------------------------------------------------------------------
-module(ehttpd_server).
-include("ehttpd.hrl").
-author("kenneth").
-export([child_spec/2, bind/4, docroot/0]).
-export([add_hook/2, run_hook/3, reload_paths/0, reload_paths/2, get_env/2, get_path/2]).
%% 获取HTTP Server
child_spec(App, Name) ->
    Env = get_env(App),
    get_child_spec(Name, Env).


%% 重新设置路由表
reload_paths(App, Name) ->
    Env = get_env(App),
    Dispatch = get_routes(Name, Env),
    cowboy:set_env(Name, dispatch, Dispatch).

reload_paths() ->
    reload_paths(?APP, ?WEBSERVER).


-spec add_hook(Key::atom(), {Mod::module(), Fun::atom()}) -> true.
add_hook(Key, {Mod, Fun}) ->
    case ehttpd_cache:lookup(Key) of
        {error, not_find} ->
            ehttpd_cache:insert(Key, [{Mod, Fun}]);
        {ok, Hooks} ->
            case lists:member({Mod, Fun}, Hooks) of
                true ->
                    true;
                false ->
                    ehttpd_cache:insert(Key, [{Mod, Fun}|Hooks])
            end
    end.

-spec run_hook(Key::atom(), Args::list(), Acc::any()) -> {ok, Acc1::any()} | {error, Reason::any()}.
run_hook(Key, Args, Acc) ->
    case ehttpd_cache:lookup(Key) of
        {error, not_find} ->
            {ok, Acc};
        {ok, Hooks} ->
            run_hook_foldl(Hooks, Args, Acc)
    end.

run_hook_foldl([], _, Acc) -> {ok, Acc};
run_hook_foldl([{Mod, Fun}|Hooks], Args, Acc) ->
    case apply(Mod, Fun, Args ++ [Acc]) of
        {error, Reason} ->
            {error, Reason};
        {stop, Acc1} ->
            {ok, Acc1};
        {ok, Acc1} ->
            run_hook_foldl(Hooks, Args, Acc1)
    end.

%% 动态加入路径
bind(Path, Mod, _Options, priv) ->
    case ehttpd_swagger:load_schema(Mod, Path, [{labels, binary}, return_maps]) of
        {ok, Schemas} ->
            Schemas;
        {error, Reason} ->
            throw({error, Reason})
    end;
bind(Path, _Mod, Options, MetaData) ->
    #{
        <<"definitions">> => proplists:get_value(<<"definitions">>, Options, #{}),
        <<"paths">> => #{
            Path => MetaData
        }
    }.

docroot() ->
    get_path(?APP, docroot).

-spec get_child_spec(Name :: atom(), #{port => inet:port_number(), acceptors => integer(), docroot => list()}) -> {ok, pid()} | {error, any()}.
get_child_spec(Name, #{
    port:= Port,
    acceptors:= Acceptors
} = Env) ->
    SSL = maps:with([cacertfile, certfile, keyfile], Env),
    IP = {0, 0, 0, 0},
    NetOpts = [{ssl, maps:to_list(SSL)}],
    {Transport, TransportOpts} = get_transport(IP, Port, NetOpts),
    TransOpts = [{num_acceptors, Acceptors}|TransportOpts],
    ExtraOpts = maps:get(cowboy_extra_opts, Env, []),
    DefaultOpts = #{
        env =>#{
            dispatch => get_routes(Name, Env)
        }
    },
    Opts = get_config(ExtraOpts, DefaultOpts),
    ranch:child_spec(Name, Transport, TransOpts, cowboy_clear, Opts).


get_transport(IP, Port, Options) ->
    Opts = [
        {ip, IP},
        {port, Port}
    ],
    case proplists:get_value(ssl, Options, []) of
        [] ->
            {ranch_tcp, Opts};
        SslOpts = [_ | _] ->
            {ranch_ssl, Opts ++ SslOpts}
    end.


get_config([], Opts) ->
    Opts;
get_config([{env, Env} | Rest], Opts) ->
    OldEnv = maps:get(env, Opts, #{}),
    OldDispatch = maps:get(dispatch, OldEnv, []),
    NewDisPatch = maps:get(dispatch, Env, []),
    DisPatch = lists:concat([OldDispatch, NewDisPatch]),
    NewEnv = maps:merge(OldEnv, maps:without([dispatch], Env)),
    get_config(Rest, Opts#{env => NewEnv#{dispatch => DisPatch}});
get_config([{Key, Value} | Rest], Opts) ->
    get_config(Rest, Opts#{Key => Value}).


get_routes(Name, #{docroot := DocRoot}) ->
    Dispatch = ehttpd_router:get_paths(Name, DocRoot),
    cowboy_router:compile([{'_', Dispatch}]).


get_env(App) ->
    lists:foldl(
        fun(Key, Acc) ->
            case get_env(App, Key) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end, #{}, [port, docroot, acceptors, cacertfile, certfile, keyfile]).
get_env(App, Key) when Key == docroot; Key == cacertfile; Key == certfile; Key == keyfile ->
    get_path(App, Key);
get_env(App, Key) ->
    get_env_value(App, Key).


get_path(App, Key) ->
    case get_env_value(App, Key) of
        undefined ->
            undefined;
        "app/" ++ Path ->
            [TargetApp, "priv/" ++ Path1]= string:split(Path, "/"),
            Dir = code:priv_dir(list_to_atom(TargetApp)),
            to_path(filename:join(Dir, Path1));
        "priv/" ++ _ = Path ->
            {file, Here} = code:is_loaded(?MODULE),
            Dir = filename:dirname(filename:dirname(Here)),
            to_path(filename:join([Dir, Path]));
        Path ->
            to_path(Path)
    end.

get_env_value(App, Key) ->
    case application:get_env(App, Key) of
        undefined -> undefined;
        {ok, ""} -> undefined;
        {ok, <<>>} -> undefined;
        {ok, Value} -> Value
    end.


to_path(Path) when is_list(Path) ->
    to_path(list_to_binary(Path), []);
to_path(Path) ->
    to_path(Path, []).
to_path(<<"../", Path/binary>>, Acc) ->
    to_path(Path, [<<"../">>|Acc]);
to_path(Path, Acc) ->
    {ok, Cwd} = file:get_cwd(),
    Path2 = lists:foldl(fun(<<"../">>, Path1) -> filename:dirname(Path1) end, Cwd, Acc),
    binary_to_list(filename:join([Path2, Path])).