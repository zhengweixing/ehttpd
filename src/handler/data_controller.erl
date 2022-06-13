-module(data_controller).

%% API
-export([swagger/1]).


swagger(ehttpd) ->
    Path = code:priv_dir(ehttpd),
    {ok, Data} = file:read_file(filename:join([Path, "tables.json"])),
    Tables = jiffy:decode(Data, [return_maps]),
    TplPath = filename:join([Path, "swagger/swagger_curd.json"]),
    case erlydtl:compile({file, TplPath}, render, [{out_dir, false}]) of
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
    <<"desc">> := Desc,
    <<"fields">> := Fields
}, Render) ->
    Fields1 =
        lists:foldl(
            fun(Field, Acc) ->
                [maps:fold(
                    fun(Key, Value, Acc1) ->
                        [{binary_to_atom(Key), Value}|Acc1]
                    end, [], Field) | Acc]
            end, [], Fields),
    Vals = [{tableName, TableName}, {desc, Desc}, {fields, Fields1}],
    {ok, IoList} = Render:render(Vals),
    jiffy:decode(unicode:characters_to_binary(IoList), [return_maps]).
