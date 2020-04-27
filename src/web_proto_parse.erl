-module(web_proto_parse).

-export([enum/1]).

%% 没有嵌套存在
enum(<<"enum", Bin/binary>>) ->
    [Name, Bin0] = binary:split(Bin, [<<"{">>]),
    [ContentBin, Bin1] = binary:split(Bin0, [<<"}">>]),
    Content = binary:split(erase_mark(ContentBin, []), [<<";">>], [global]),
    {erase_space(Name), do_enum(Content, []), Bin1};
enum(<<_:8, Bin/binary>>) ->
    enum(Bin);
enum(_) ->
    not_found.

do_enum([Bin|T], Res) ->
    case erase_space(Bin) of
        <<>> ->
            do_enum(T, Res);
        Bin1 ->
            [K, V] = binary:split(Bin1, [<<"=">>]),
            do_enum(T, [{K, V} | Res])
    end;
do_enum([], Res) ->
    lists:reverse(Res).

erase_space(Bin) ->
    binary:replace(Bin, [<<" ">>, <<"\n">>, <<"\r">>],<<"">>,[global]).

erase_mark(<<"//", Bin/binary>>, Res) ->
    case binary:split(Bin, [<<"\n">>]) of
        [_, Bin1] ->
            erase_mark(Bin1, Res);
        [Bin1] ->
            erase_mark(Bin1, Res)
    end;
erase_mark(<<H:8, Bin/binary>>, Res) ->
    erase_mark(Bin, [<<H:8>> | Res]);
erase_mark(<<>>, Res) ->
    list_to_binary(lists:reverse(Res)).
