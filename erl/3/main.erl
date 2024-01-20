-module(main).
-export([main/0]).

main() ->
    {ok, FileContent} = file:read_file("./../../data/3.txt"),
    {_RowWidth, _} = binary:match(FileContent, <<"\n">>),
    {Numbers, Symbols} = parse(FileContent),
    io:format("~w~n~w~n", [Numbers, Symbols]).

parse(Content) ->
    parse(Content, 0, [], []).

parse(<<Number:1/binary, Rest/binary>>, Cursor, Numbers, Symbols)
  when Number >= <<$0>>, Number =< <<$9>> ->
    {NextContent, Num} = parse_number(Rest, Number),
    EndCursor = Cursor + byte_size(Num),
    parse(NextContent, EndCursor, [{Cursor, EndCursor, binary_to_integer(Num)} | Numbers], Symbols);
parse(<<".", Rest/binary>>, Cursor, Numbers, Symbols) ->
    parse(Rest, Cursor + 1, Numbers, Symbols);
parse(<<Symbol:1/binary, Rest/binary>>, Cursor, Numbers, Symbols) ->
    parse(Rest, Cursor + 1, Numbers, [{Cursor, Symbol} | Symbols]);
parse(<<>>, _Cursor, Numbers, Symbols) ->
    {Numbers, Symbols}.

parse_number(<<Number:1/binary, Rest/binary>>, Acc)
  when Number >= <<$0>>, Number =< <<$9>> ->
    parse_number(Rest, <<Acc/binary, Number/binary>>);
parse_number(Content, Acc) ->
    {Content, Acc}.

