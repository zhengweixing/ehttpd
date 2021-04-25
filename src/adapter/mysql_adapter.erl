%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 25. 4月 2021 5:20 下午
%%%-------------------------------------------------------------------
-module(mysql_adapter).
-author("weixingzheng").

%% API
-export([get_schemas/0]).

get_schemas() ->
    {ok, #{<<"results">> => [
        #{
            <<"className">> => <<"Device">>,
            <<"fields">> => #{
                <<"name">> => #{
                    <<"type">> => <<"String">>
                },
                <<"product">> => #{
                    <<"targetClass">> => <<"Product">>,
                    <<"type">> => <<"Pointer">>
                },
                <<"dev">> => #{
                    <<"type">> => <<"String">>,
                    <<"required">> => true
                },
                <<"status">> => #{
                    <<"type">> => <<"String">>
                },
                <<"isEnable">> => #{
                    <<"type">> => <<"Boolean">>
                }
            }
        }
    ]}}.