-module(ehttpd_app).

-behaviour(application).
-include("ehttpd.hrl").

-emqx_plugin(?MODULE).

%% Application callbacks
-export([start/2, stop/1]).



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
