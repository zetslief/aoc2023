-module(main).
-export([main/0]).

main() ->
    {ok, Content} = file:read_file("./../../data/2.txt"),
    Lines = binary:split(Content, <<"\n">>, [global, trim]), 
    Games = lists:map(fun parse_game/1, Lines),
    FirstResult = first(Games),
    io:format("First: ~w~n", [FirstResult]),
    SecondResult = second(Games),
    io:format("Second: ~w~n", [SecondResult]).

first(Games) ->
    CalculateSet = fun (Subset) ->
        lists:foldl(fun ({Number, Color}, {Red, Green, Blue}) ->
            case Color of
                red -> {Red + Number, Green, Blue};
                green -> {Red, Green + Number, Blue};
                blue -> {Red, Green, Blue + Number}
            end
        end,
        {0, 0, 0},
        Subset)
    end,
    [RedLimit, GreenLimit, BlueLimit] = [12, 13, 14],
    SubsetWithinLimits = fun ({Red, Green, Blue}) ->
        Red =< RedLimit andalso Green =< GreenLimit andalso Blue =< BlueLimit
    end,
    EvaluateGame = fun ({GameId, Set}) ->
        case lists:all(SubsetWithinLimits, lists:map(CalculateSet, Set)) of
            true -> GameId;
            false -> 0
        end
    end,
    CorrectGames = lists:map(EvaluateGame, Games),
    lists:foldl(fun (Elem, Acc) -> Elem + Acc end, 0, CorrectGames).

second(Games) ->
    EvaluateSubset = fun ({Number, Color}, {RedAcc, GreenAcc, BlueAcc}) ->
        case Color of 
            red -> {if Number > RedAcc -> Number; true -> RedAcc end, GreenAcc, BlueAcc};
            green -> {RedAcc, if Number > GreenAcc -> Number; true -> GreenAcc end, BlueAcc};
            blue -> {RedAcc, GreenAcc, if Number > BlueAcc -> Number; true -> BlueAcc end}
        end
    end,
    EvaluateSet = fun (Set) ->
        Subsets = lists:flatten(Set),
        {Red, Green, Blue} = lists:foldl(EvaluateSubset, {0, 0, 0}, Subsets),
        Red * Green * Blue
    end,
    EvaluateGame = fun ({_GameId, Set}) ->
        EvaluateSet(Set)
    end,
    lists:sum(lists:map(EvaluateGame, Games)).

parse_game(Content) ->
    {GameId, Rest} = game_id(Content),
    Sets = parse_sets(Rest),
    {GameId, Sets}.

game_id(Content) ->
    {PrefixSymbol, PrefixSymbolLength} = binary:match(Content, <<" ">>),
    {GameIdEnd, _GameIdEndLength} = binary:match(Content, <<":">>),
    PrefixLength = PrefixSymbol + PrefixSymbolLength,
    GameIdLength = GameIdEnd - PrefixLength,
    <<_Prefix:PrefixLength/binary, GameId:GameIdLength/binary, Rest/binary>> = Content,
    {binary_to_integer(GameId), Rest}.

parse_sets(Content) ->
    Sets = binary:split(Content, <<";">>, [global, trim]),
    parse_sets(Sets, []).

parse_sets([SetContent | Tail], Acc) ->
    Set = parse_set(SetContent),
    parse_sets(Tail, [Set | Acc]);
parse_sets([], Acc) ->
    Acc.

parse_set(<<":", Rest/binary>>) ->
    parse_set(Rest);
parse_set(<<" ", Rest/binary>>) ->
    parse_set(Rest);
parse_set(SetContent) ->
    Subsets = binary:split(SetContent, <<", ">>, [global]),
    lists:map(fun parse_subset/1, Subsets).

parse_subset(Subset) ->
    {DelimiterIndex, DelimiterLength} = binary:match(Subset, <<" ">>),
    <<Number:DelimiterIndex/binary, _Delimiter:DelimiterLength/binary, Color/binary>> = Subset,
    {
         binary_to_integer(Number),
         case Color of
             <<"red">> -> red;
             <<"green">> -> green;
             <<"blue">> -> blue
         end
    }.
