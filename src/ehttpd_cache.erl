%%%-------------------------------------------------------------------
%%% @author weixingzheng
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%% 可以接入redis
%%% @end
%%% Created : 26. 10月 2020 8:49 下午
%%%-------------------------------------------------------------------
-module(ehttpd_cache).
-author("weixingzheng").
-define(DB, ?MODULE).
%% API
-export([start/0, match/1, lookup/1, insert/2, get/1, set/3]).

start() ->
    ets:new(?DB, [public, named_table, ordered_set, {write_concurrency, true}, {read_concurrency, true}]),
    ok.

match(Pattern) ->
    case ets:match(?DB, Pattern) of
        [] -> {error, empty};
        Matchs -> {ok, Matchs}
    end.

lookup(Key) ->
    case ets:lookup(?DB, Key) of
        [] -> {error, not_find};
        [{Key, Value} | _] -> {ok, Value};
        [Value | _] -> {ok, Value}
    end.

insert(Key, Value) ->
    ets:insert(?DB, {Key, Value}).


%% 可以接入redis
get(Key) ->
    call(ecache, get, [Key]).

%% 可以接入redis
set(Key, Value, TTL) ->
    call(ecache, set, [Key, Value, TTL]).


call(Mod, Fun, Args) ->
    try
        apply(Mod, Fun,  Args)
    catch
        _:Reason  ->
            {error, Reason}
    end.