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
    start_http(),
    {ok, Sup}.

stop(_State) ->
    ok.

start_http() ->
    case application:get_env(ehttpd, port, undefined) of
        undefined -> ok;
        Port ->
            case ehttpd_server:start(default, Port, ehttpd_config:get_env()) of
                {ok, _} -> ok;
                Err ->
                    logger:error("Failed to start http server: ~p", [Err])
            end
    end.
