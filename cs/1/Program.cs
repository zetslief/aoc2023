// First: 54388
// Second: 53515

var content = File.ReadAllText("./../../data/1.txt");

Console.WriteLine($"First: {First(content)}");
Console.WriteLine($"Second: {Second(content)}");

static int First(string content)
{
    static IEnumerable<string> SplitLines(string lines)
        => lines.Split(Environment.NewLine).Where(l => !string.IsNullOrEmpty(l));

    static (int? first, int? last) ProcessNewNumber(ReadOnlySpan<char> @new, int? first, int? last)
        => int.TryParse(@new, out var value)
            ? first.HasValue
                ? (first, value)
                : (value, value)
            : (first, last);

    static int SumFirstAndLastNumbers(string line)
    {
        return ProcessLine(line[..1], line[1..], null, null) switch
        {
            var (first, last) when first is not null && last is not null
                => first.Value * 10 + last.Value,
            var (first, last) => throw new InvalidOperationException(
                $"Failed to parse: '{line}'. First: '{first}' Last: '{last}'."),
        };
    }

    static (int? first, int? last) ProcessLine(ReadOnlySpan<char> Head, ReadOnlySpan<char> Tail, int? first, int? last)
    {
        (first, last) = ProcessNewNumber(Head, first, last);
        return Tail.Length == 0
            ? (first, last)
            : ProcessLine(Tail[..1], Tail[1..], first, last);
    }

    return SplitLines(content)
        .Select(SumFirstAndLastNumbers)
        .Sum();
}

static int Second(string content)
{
    static IEnumerable<string> SplitLines(string lines)
        => lines.Split(Environment.NewLine)
            .Where(l => !string.IsNullOrEmpty(l));

    static int SumFirstAndLastNumbers(string line)
    {
        return ProcessLine(line[..Math.Min(5, line.Length)], line[1..], null, null) switch
        {
            var (first, last) when first is not null && last is not null
                => first.Value * 10 + last.Value,
            var otherwise => throw new InvalidOperationException(
                $"Failed to parse: '{line}'. Values: '{otherwise}'."),
        };
    }

    static (int? first, int? last) ProcessLine(ReadOnlySpan<char> head, ReadOnlySpan<char> tail, int? first, int? last)
    {
        int? result = int.TryParse(head[..1], out var digit)
            ? digit
            : ProcessChunk(head);
        return tail.Length == 0
            ? (first.HasValue ? first : result, result.HasValue ? result : last)
            : ProcessLine(
                tail[..Math.Min(5, tail.Length)],
                tail[1..],
                first.HasValue ? first : result,
                result.HasValue ? result : last);
    }

    static int? ProcessChunk(ReadOnlySpan<char> chunk)
    {
        if (chunk.Length > 0 && char.IsDigit(chunk[0])) return null;
        return chunk.Length switch
        {
            < 3 => null,
            < 6 => Enum.TryParse<Numbers>(chunk.ToString(), out var value) ? (int)value : ProcessChunk(chunk.Slice(0, chunk.Length - 1)),
            _ => throw new InvalidOperationException("Chunk cannot be more than 5."),
        };
    }

    return SplitLines(content)
        .Select(SumFirstAndLastNumbers)
        .Sum();
}

enum Numbers
{
    one = 1, two = 2, three = 3,
    four = 4, five = 5, six = 6,
    seven = 7, eight = 8, nine = 9
}