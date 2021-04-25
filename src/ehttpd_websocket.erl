%%%-------------------------------------------------------------------
%%% @author zwx
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. 三月 2019 3:39
%%%-------------------------------------------------------------------
-module(ehttpd_websocket).
-author("zwx").


-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([terminate/3]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    {ok, State}.

websocket_handle({text, Bin}, State) ->
    io:format("WS:~p~n", [Bin]),
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info(Info, State) ->
    {reply, Info, State}.

terminate(_Reason, _Req, _State) ->
    ok.