-module(main).
-export([main/0]).

main() ->
    {ok, FileContent} = file:read_file("./../../data/4.txt"),
    Games = parse(FileContent),
    io:format("First: ~w~n", [first(Games)]),
    io:format("Second: ~w~n", [second(Games)]).

first(Games) ->
    FindNumbersThatWon = fun ({_, Wins, Numbers}) -> findNumbersThatWon(Wins, Numbers) end,
    lists:sum(
      lists:map(
          fun calculatePoints/1,
          lists:map(
            FindNumbersThatWon,
            Games))).

second(Games) ->
    not_implemented.

calculatePoints(Numbers) ->
    math:floor(math:pow(2, length(Numbers) - 1)).

findNumbersThatWon(Wins, Numbers) ->
    lists:filter(hasNumberWon(Wins), Numbers).

hasNumberWon(Wins) ->
    fun (Number) -> sets:is_element(Number, Wins) end.

parse(Content) ->
    parse(Content, []).

parse(<<>>, Results) ->
    Results;
parse(Content, Results) ->
    {Header, HeaderRest} = parse_header(Content),
    {Wins, WinRest} = parse_numbers(HeaderRest, <<"|">>),
    {Available, AvailableRest} = parse_numbers(WinRest, <<"\n">>),
    parse(AvailableRest, [{Header, sets:from_list(Wins), Available} |  Results]).

parse_header(Content) ->
    parse_header(Content, <<>>).

parse_header(<<Digit:1/binary, Rest/binary>>, Acc)
  when Digit >= <<$0>>, Digit =< <<$9>> ->
    parse_header(Rest, <<Acc/binary, Digit/binary>>);
parse_header(<<":", Rest/binary>>, Acc) ->
    {binary_to_integer(Acc), Rest};
parse_header(<<_Symbol:1/binary, Rest/binary>>, Acc) ->
    parse_header(Rest, Acc).

parse_numbers(Content, Stop) ->
    parse_numbers(Content, Stop, <<>>, []).

parse_numbers(<<Digit:1/binary, Rest/binary>>, Stop, NumAcc, Nums)
  when Digit >= <<$0>>, Digit =< <<$9>> ->
    parse_numbers(Rest, Stop, <<NumAcc/binary, Digit/binary>>, Nums);
parse_numbers(<<Symbol:1/binary, Rest/binary>>, Stop, NumAcc, Nums) ->
    case {Symbol, NumAcc} of
        {Stop, <<>>} -> {Nums, Rest};
        {Stop, Num} -> {[binary_to_integer(Num) | Nums], Rest};
        {_, <<>>} -> parse_numbers(Rest, Stop, <<>>, Nums);
        {_, Num} -> parse_numbers(Rest, Stop, <<>>, [binary_to_integer(Num) | Nums])
    end.
