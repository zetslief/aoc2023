-module (aoc).
-export ([main/0]).

main() -> 
    case file:read_file("data.txt") of
         {ok, Content} -> split_lines(Content);
         {error, Error} -> io:write(Error)
    end.

split_lines(Content) ->
    case binary:split(Content, <<"\n">>) of
        [H, T] -> split_chars(H), split_lines(T);
        [C] -> split_chars(C);
        [] -> theend
    end.
