-module(main).
-export([main/0]).

main() ->
    {ok, FileContent} = file:read_file("./../../data/4.txt"),
    parse(FileContent),
    io:format("~w~n", [FileContent]).

parse(Content) ->
    {Header, Rest} = parse_header(Content),
    {Wins, Rest} = parse_numbers(Rest, <<"|">>),
    {Availalble, Rest} = parse_numbers(Rest, <<"\n">>),
    {Header, Wins, Available}.

parse_header(Content) ->
    parse_header(Content, <<>>).

parse_header(<<Digit:1/binary, Rest>>, Acc)
  when Digit >= <<$0>>, Digit =< <<$9>> ->
    parse_header(Rest, <<Acc, Digit>>);
parse_header(<<":", Rest>>, Acc) ->
    {binary_to_number(Acc), Rest}.

parse_numbers(Content, Stop) ->
    parse_numbers(Content, Stop, <<>>, []).

parse_numbers(<<Digit:1/binary, Rest>>, Stop, NumAcc, Nums)
  when Digit >= <<$0>>, Digit =< <<$9>> ->
    parse_numbers(Rest, Stop, <<NumAcc, Digit>>, Nums);
parse_numbers(<<Symbol:1/binary, Rest>>, Stop, NumAcc, Nums) ->
    case {Symbol, NumAcc} of
        {Stop, <<>>} -> {Nums, Rest};
        {Stop, Num} -> {[binary_to_integer(Num) | Nums], Rest};
        {_, <<>>} -> parse_numbers(Rest, Stop, <<>>, Nums);
        {_, Num} -> parse_numbers(Rest, Stop, <<>>, [binary_to_integer(Num) | Nums])
    end.
