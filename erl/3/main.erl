-module(main).
-export([main/0]).

main() ->
    {ok, FileContent} = file:read_file("./../../data/3.txt"),
    {Width, _} = binary:match(FileContent, <<"\n">>),
    RowWidth = Width + 1,
    {Numbers, Symbols} = parse(FileContent),
    drawNumbers(Numbers, Symbols, RowWidth),
    drawSymbols(Symbols, RowWidth),
    IsConnected = fun (Number) -> is_number_connected(Number, Symbols, RowWidth) end,
    SumOfConnected = lists:sum(lists:map(
                fun ({_, _, Number}) -> Number end,
                lists:filter(IsConnected, Numbers))),
    io:format("First: ~w~n", [SumOfConnected]).

drawNumber({StartIndex, EndIndex, Number}, Symbols, RowWidth) ->
    Start = from_flat_index(StartIndex, RowWidth),
    End = from_flat_index(EndIndex, RowWidth),
    IsConnected = is_number_connected({StartIndex, EndIndex, Number}, Symbols, RowWidth),
    io:format("~w [~w, ~w] -> ~w~n", [Number, Start, End, IsConnected]).
drawNumbers(Numbers, Symbols, Width) ->
    lists:foreach(
      fun (Number) -> drawNumber(Number, Symbols, Width) end,
      Numbers).

drawSymbol({Index, Symbol}, Width) ->
    io:format("~w: ~s~n", [from_flat_index(Index, Width), Symbol]).
drawSymbols(Symbols, Width) ->
    lists:foreach(
      fun (Symbol) -> drawSymbol(Symbol, Width) end,
      Symbols).


parse(Content) ->
    parse(Content, 0, [], []).

parse(<<Number:1/binary, Rest/binary>>, Cursor, Numbers, Symbols)
  when Number >= <<$0>>, Number =< <<$9>> ->
    {NextContent, Num} = parse_number(Rest, Number),
    EndCursor = Cursor + byte_size(Num),
    parse(NextContent, EndCursor, [{Cursor, EndCursor, binary_to_integer(Num)} | Numbers], Symbols);
parse(<<Skip:1/binary, Rest/binary>>, Cursor, Numbers, Symbols)
  when Skip == <<".">>; Skip == <<"\n">>->
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

from_flat_index(FlatIndex, RowWidth) ->
    {FlatIndex div RowWidth, FlatIndex rem RowWidth}.

is_number_connected(Number, Symbols, RowWidth) ->
    {NumberIndex, NumberEndIndex, _} = Number,
    lists:any(
      fun ({SymbolIndex, _Symbol}) ->
              is_number_connected(NumberIndex, NumberEndIndex, SymbolIndex, RowWidth) end,
      Symbols).

is_number_connected(NumberIndex, NumberEndIndex, SymbolIndex, RowWidth) ->
    {NumberRow, NumberColumn} = from_flat_index(NumberIndex, RowWidth),
    {_NumberEndRow, NumberEndColumn} = from_flat_index(NumberEndIndex, RowWidth),
    {SymbolRow, SymbolColumn} = from_flat_index(SymbolIndex, RowWidth),
    WithinOneRow = (NumberRow + 1 >= SymbolRow) and (NumberRow - 1 =< SymbolRow),
    StartWithinOneColumn = (NumberColumn - 1) =< SymbolColumn,
    EndWihinOneColumn = NumberEndColumn >= SymbolColumn,
    WithinOneRow and StartWithinOneColumn and EndWihinOneColumn.
