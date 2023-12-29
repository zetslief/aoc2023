-module (aoc1).
-export ([main/0]).

main() -> 
    Lines = case file:read_file("./../../data/1.txt") of
         {ok, Content} -> split_lines(Content);
         {error, Error} -> io:write(Error)
    end,
    Print = fun({Line, First, Last}) ->
        io:format("~s: ~w(~s) ~w(~s)~n", [Line, First, First, Last, Last])
    end,
    ConvertToNumbers = fun({_Line, First, Last}) ->
       {binary_to_integer(First), binary_to_integer(Last)}
    end,
    Calculate = fun ({First, Last}, Acc) ->
        Acc + (First * 10 + Last)
    end,
    Numbers = process_lines(Lines),
    lists:foreach(Print, Numbers),
    lists:foldl(
      Calculate,
      0,
      lists:map(ConvertToNumbers, Numbers)
    ).


split_lines(Content) ->
    split_lines(Content, []).

split_lines(Content, Acc) ->
    case binary:split(Content, <<"\n">>) of
        [H, T] -> split_lines(T, [H | Acc]);
        [<<>>] -> Acc
    end.

process_lines(Lines) ->
    process_lines(Lines, []).

process_lines([Line | Rest], Acc) ->
    {First, Last} = find_numbers(Line),
    process_lines(Rest, [{Line, First, Last} | Acc]);

process_lines([], Acc) ->
    Acc.

find_numbers(Line) when is_binary(Line) ->
    find_numbers(Line, {}).

find_numbers(<<Char:1/binary, Rest/binary>>, Values) when Char >= <<$0>>, Char =< <<$9>> ->
    find_numbers(Rest, values(Char, Values));
find_numbers(<<_Char:1/binary, Rest/binary>>, Values) ->
    find_numbers(Rest, Values);
find_numbers(<<>>, {First, Last}) ->
    {First, Last}.

values(Number, {}) ->
    {Number, Number};
values(Number, {First, _Last}) ->
    {First, Number}.
