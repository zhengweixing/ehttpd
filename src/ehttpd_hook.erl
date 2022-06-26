-module(ehttpd_hook).

%% API
-export([add/2, run/3]).

-spec add(Key :: atom(), {Mod :: module(), Fun :: atom()}) -> true.
add(Key, {Mod, Fun}) ->
    case ehttpd_cache:lookup(Key) of
        {error, notfound} ->
            ehttpd_cache:insert(Key, [{Mod, Fun}]);
        {ok, Hooks} ->
            case lists:member({Mod, Fun}, Hooks) of
                true ->
                    true;
                false ->
                    ehttpd_cache:insert(Key, [{Mod, Fun} | Hooks])
            end
    end.

-spec run(Key :: atom(), Args :: list(), Acc :: any()) ->
    {ok, Acc1 :: any()}.
run(Key, Args, Acc) ->
    case ehttpd_cache:lookup(Key) of
        {error, notfound} ->
            {ok, Acc};
        {ok, Hooks} ->
            run_foldl(Hooks, Args, Acc)
    end.

run_foldl([], _, Acc) -> {ok, Acc};
run_foldl([{Mod, Fun} | Hooks], Args, Acc) ->
    case apply(Mod, Fun, Args ++ [Acc]) of
        ok ->
            run_foldl(Hooks, Args, Acc);
        {ok, Acc1} ->
            run_foldl(Hooks, Args, Acc1);
        {stop, Acc1} ->
            {ok, Acc1}
    end.