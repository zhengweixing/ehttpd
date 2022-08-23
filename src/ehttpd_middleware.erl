%%%-------------------------------------------------------------------
%%% @author yqfclid
%%% @copyright (C) 2022, yuqinfeng17@gmail.com
%%% @doc
%%%
%%% @end
%%% Created : 14. 8月 2022 下午11:15
%%%-------------------------------------------------------------------
-module(ehttpd_middleware).
-author("yqfclid").

%% API
-export([execute/2]).

%%%===================================================================
%%% API
%%%===================================================================
execute(Req, Env) ->
    case ehttpd_hook:run('http.middleware', [], {Req, Env}) of
        {ok, {NReq, NEnv}} ->
            {ok, NReq, NEnv};
        {error, Reason} ->
            logger:error("error execute request ~p, ~p hook failed: ~p",
                [Req, Env, Reason]),
            {ok, Req, Env}
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================
