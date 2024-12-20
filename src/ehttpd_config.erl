%%%-------------------------------------------------------------------
%%% @author kenneth
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. 12月 2024 19:52
%%%-------------------------------------------------------------------
-module(ehttpd_config).
-author("kenneth").
-include("ehttpd.hrl").

%% API
-export([get_env/0, get_env/1]).

-define(KEYS, [
    swagger, expire, rewrite, debug, 
    docroot, cacertfile, certfile,
    keyfile, access_control_allow_headers
]).

get_env() ->
    get_env(?APP).

get_env(App) ->
    lists:foldl(
        fun(Key, Acc) ->
            case application:get_env(App, Key, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end, #{}, ?KEYS).

