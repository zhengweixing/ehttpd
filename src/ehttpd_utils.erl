%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 25. 4月 2021 4:37 下午
%%%-------------------------------------------------------------------
-module(ehttpd_utils).
-author("weixingzheng").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([query_table/4]).


query_table(Tab, PageNo, PageSize, RowFun) ->
    Qh = qlc:q([R || R <- ets:tab2list(Tab)]),
    Cursor = qlc:cursor(Qh),
    case PageNo > 1 of
        true -> qlc:next_answers(Cursor, (PageNo - 1) * PageSize);
        false -> ok
    end,
    Rows = qlc:next_answers(Cursor, PageSize),
    qlc:delete_cursor(Cursor),
    [RowFun(Row) || Row <- Rows].