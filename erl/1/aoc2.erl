-module (aoc2).
-export ([main/0, process_line/1]).

main() ->
    LineProcessor = fun (Line) ->
        {First, Last} = process_line(Line),
        {Line, First, Last}
    end,
    Print = fun ({Line, First, Last}) ->
        io:format("~s: ~w(~s) | ~w(~s)~n", [Line, First, First, Last, Last])
    end,
    Convert = fun ({_Line, First, Last}) ->
        {binary_to_integer(First), binary_to_integer(Last)}
    end,
    Calculate = fun({First, Last}, Acc) ->
        Acc + (First * 10 + Last)
    end,
    {ok, FileContent} = file:read_file("./../../data/1.txt"),
    Lines = binary:split(FileContent, <<"\n">>, [global, trim]),
    ProcessedLines = lists:map(LineProcessor, Lines),
    lists:foreach(Print, ProcessedLines),
    Result = lists:foldl(Calculate, 0, lists:map(Convert, ProcessedLines)),
    {ok, Result}.

process_line(Line) ->
    process_line(Line, {}).

process_line(<<Char:1/binary, Rest/binary>>, Values) when Char >= <<$0>>, Char =< <<$9>> ->
    process_line(Rest, value(Char, Values));
process_line(<<"one", Rest/binary>>, Values) ->
    process_line(<<"ne", Rest/binary>>, value(<<$1>>, Values));
process_line(<<"two", Rest/binary>>, Values) ->
    process_line(<<"wo", Rest/binary>>, value(<<$2>>, Values));
process_line(<<"three", Rest/binary>>, Values) ->
    process_line(<<"hree", Rest/binary>>, value(<<$3>>, Values));
process_line(<<"four", Rest/binary>>, Values) ->
    process_line(<<"our", Rest/binary>>, value(<<$4>>, Values));
process_line(<<"five", Rest/binary>>, Values) ->
    process_line(<<"ive", Rest/binary>>, value(<<$5>>, Values));
process_line(<<"six", Rest/binary>>, Values) ->
    process_line(<<"ix", Rest/binary>>, value(<<$6>>, Values));
process_line(<<"seven", Rest/binary>>, Values) ->
    process_line(<<"even", Rest/binary>>, value(<<$7>>, Values));
process_line(<<"eight", Rest/binary>>, Values) ->
    process_line(<<"ight", Rest/binary>>, value(<<$8>>, Values));
process_line(<<"nine", Rest/binary>>, Values) ->
    process_line(<<"ine", Rest/binary>>, value(<<$9>>, Values));
process_line(<<_MismatchedChar:1/binary, Rest/binary>>, Values) ->
    process_line(Rest, Values);
process_line(<<>>, {First, Last}) ->
    {First, Last}.

value(Number, {}) ->
    {Number, Number};
value(Number, {First, _Last}) ->
    {First, Number}.
