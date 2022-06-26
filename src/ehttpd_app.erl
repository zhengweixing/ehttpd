-module(ehttpd_app).

-behaviour(application).
-include("ehttpd.hrl").

%% Application callbacks
-export([start/2, stop/1]).



%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    {ok, Sup} = ehttpd_sup:start_link(),
    Port = application:get_env(ehttpd, port, 6080),
    {ok, _} = ehttpd_server:start(default, Port, get_env()),
    {ok, Sup}.

stop(_State) ->
    ok.


get_env() ->
    lists:foldl(
        fun(Key, Acc) ->
            case application:get_env(ehttpd, Key, undefined) of
                undefined -> Acc;
                Value -> Acc#{Key => Value}
            end
        end, #{}, [swagger, expire, debug, docroot, cacertfile, certfile, keyfile, access_control_allow_headers]).
