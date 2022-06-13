-module(data_controller).

%% API
-export([swagger/1]).


swagger(ehttpd) ->
    Path = code:priv_dir(ehttpd),
    {ok, Data} = file:read_file(filename:join([Path, "tables.json"])),
    Tables = jiffy:decode(Data, [return_maps]),
    TplPath = filename:join([Path, "swagger/swagger_curd.json"]),
    case erlydtl:compile({file, TplPath}, render, [{out_dir, false}, [{api, record_info(fields, api)}]]) of
        {ok, Render} ->
            lists:foldl(
                fun(Table, Acc) ->
                    [create_swagger(Table, Render) | Acc]
                end, [], Tables);
        error ->
            {error, create_error}
    end.

create_swagger(#{
    <<"tableName">> := TableName,
    <<"fields">> := Fields
}, Render) ->
    Vals = [{tableName, TableName}, [tag, TableName], [fields, Fields]],
    {ok, IoList} = Render:render(Vals),
    jiffy:decode(unicode:characters_to_binary(IoList), [return_maps]).
