-module(web_proto_parse).

-export([enum/1]).

%% 没有嵌套存在
enum(<<"enum", Bin/binary>>) ->
    [Name, Bin0] = binary:split(Bin, [<<"{">>]),
    [ContentBin, Bin1] = binary:split(Bin0, [<<"}">>]),
    Content = binary:split(ContentBin, [<<";">>], [global]),
    {erase_space(Name), [kv_change(C) || C <- Content], Bin1};
enum(<<_:32, Bin/binary>>) ->
    enum(Bin);
enum(_) ->
    not_found.


kv_change(Bin) ->
    [K, V] = binary:split(Bin, [<<"=">>]),
    {erase_space(K), erase_space(V)}.

erase_space(Bin) ->
    binary:replace(Bin, [<<" ">>, <<"\n">>, <<"\r">>],<<"">>,[global]).
