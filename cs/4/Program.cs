ReadOnlySpan<char> fileContent = File.ReadAllText("./../../data/4.txt")
    .AsSpan();

Console.WriteLine(fileContent.ToString());

var cards = ParseCards(fileContent);

First(cards);
Second(cards);

static void First(IReadOnlyCollection<Card> cards)
{
    var first = cards.Select(EvaluateCard).Sum();
    Console.WriteLine($"First: {first}");
}

static void Second(IReadOnlyList<Card> cards)
{
    var storage = new int[cards.Count];
    Array.Fill(storage, 1);
    for (int cardIndex = 0; cardIndex < cards.Count; ++cardIndex)
    {
        var points = storage[cardIndex];
        var match = EvaluateNumberOfMatches(cards[cardIndex]);
        for (int matchIndex = 0; matchIndex < match; ++matchIndex)
        {
            var winCardIndex = cardIndex + 1 + matchIndex; // +1 is required because we do not want to give points to current card card!
            if (winCardIndex == cards.Count) break;
            storage[winCardIndex] += points;
        }
    }
    Console.WriteLine($"Second: {storage.Sum()}");
}

static int EvaluateCard(Card card)
{
    var match = EvaluateNumberOfMatches(card);
    return match == 0 ? 0 : 1 << (match - 1);
}

static int EvaluateNumberOfMatches(Card card)
    => card.AvailableNumbers.Count(card.WinNumbers.Contains);

static IReadOnlyList<Card> ParseCards(ReadOnlySpan<char> content)
{
    var result = new List<Card>();

    var cursor = 0;
    while (content.Length > cursor)
    {
        result.Add(ParseCard(content, ref cursor));
        ++cursor; // skipping new line symbol;
    }

    return result;
}

static Card ParseCard(ReadOnlySpan<char> content, ref int cursor) => new(
    ParseCardId(content, ref cursor),
    ParseWinNumbers(content, ref cursor),
    ParseAvailableNumbers(content, ref cursor)
);

static int ParseCardId(ReadOnlySpan<char> content, ref int cursor)
{
    var contentAfterCursor = content[cursor..];
    var idStart = contentAfterCursor.IndexOf(' ');
    if (idStart < 0) throw Fail(content, cursor, "No space found before card id!");
    var idEnd = contentAfterCursor.IndexOf(':');
    if (idEnd < 0) throw Fail(content, cursor, "No colon found after card id!");
    idStart += cursor;
    idEnd += cursor;
    cursor = idEnd;
    return int.TryParse(content[idStart..idEnd], out var id)
        ? id
        : throw Fail(content, cursor, "Failed to parse card id!");
}

static HashSet<int> ParseWinNumbers(ReadOnlySpan<char> content, ref int cursor)
{
    var winNumbersEnd = content[cursor..].IndexOf('|');
    if (winNumbersEnd <= 0)
        throw Fail(content, cursor, "Failed to find splitter between win and available numbers.");
    winNumbersEnd += cursor;
    var storage = new HashSet<int>();
    ParseNumbers(content[..winNumbersEnd], ref cursor, number => storage.Add(number));
    return storage;
}

static IReadOnlyCollection<int> ParseAvailableNumbers(ReadOnlySpan<char> content, ref int cursor)
{
    var lineEnd = content[cursor..].IndexOf('\n');
    if (lineEnd < 0) throw Fail(content, cursor, "Failed to find end of line while parsing available numbers!");
    lineEnd += cursor;
    var storage = new List<int>();
    ParseNumbers(content[..lineEnd], ref cursor, storage.Add);
    return storage;
}

static void ParseNumbers(ReadOnlySpan<char> content, ref int cursor, Action<int> accumulate)
{
    do
    {
        // skip redundant symbols
        while (content.Length > cursor && !char.IsDigit(content[cursor])) ++cursor;
        if (cursor == content.Length) break;
        accumulate(ParseNumber(content, ref cursor));
    }
    while (cursor < content.Length);
}

static int ParseNumber(ReadOnlySpan<char> content, ref int cursor)
{
    var start = cursor;
    ++cursor; // number parsing has to start from a digit, so checks for the first one can be safely skipped.
    while (content.Length > cursor && char.IsDigit(content[cursor])) ++cursor;
    return int.TryParse(content[start..cursor], out var number)
        ? number
        : throw Fail(content, cursor, $"Failed to parse number: '{content[start..cursor]}'");
}

static Exception Fail(ReadOnlySpan<char> content, int cursor, string message)
{
    Console.Error.WriteLine($"ERROR: {message}");
    Console.Error.WriteLine($"ERROR: at {cursor}");
    int from = Math.Max(0, cursor - 10);
    int until = Math.Min(content.Length, cursor + 10);
    Console.Error.WriteLine($"ERROR: {content[from..until]}");
    return new InvalidOperationException(message);
}

record Card(int Id, HashSet<int> WinNumbers, IReadOnlyCollection<int> AvailableNumbers)
{
    public override string ToString()
        => $"Card {Id}: {string.Join(',', WinNumbers)} | {string.Join(',', AvailableNumbers)}";
}
