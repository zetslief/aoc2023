-module (aoc).
-export ([main/0]).

main() -> 
    Lines = case file:read_file("./../../data/1.txt") of
         {ok, Content} -> split_lines(Content);
         {error, Error} -> io:write(Error)
    end,
    process_lines(Lines).


split_lines(Content) ->
    split_lines(Content, []).

split_lines(Content, Acc) ->
    case binary:split(Content, <<"\n">>) of
        [H, T] -> split_chars(H), split_lines(T, [H | Acc]);
        [<<>>] -> Acc
    end.

process_lines(Lines) ->
    process_lines(Lines, 0).

process_lines([H | T], Counter) ->
    split_chars(H),
    process_lines(T, Counter + 1);
process_lines([], Counter) ->
    {ok, Counter}.

split_chars(C) ->
    io:format("~s~n", [C]).
