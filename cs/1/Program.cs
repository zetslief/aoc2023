// First: 54388
// Second: 53515

var content = File.ReadAllText("data.txt");

Console.WriteLine($"First: {First(content)}");
Console.WriteLine($"Second: {Second(content)}");

static int First(string content) {
    static IEnumerable<string> SplitLines(string lines)
        => lines.Split(Environment.NewLine).Where(l => !string.IsNullOrEmpty(l));

    static (int? first, int? last) ProcessNewNumber(ReadOnlySpan<char> @new, int? first, int? last)
        => int.TryParse(@new, out var value)
            ? first.HasValue
                ? (first, value) 
                : (value, value)
            : (first, last);

    static int SumFirstAndLastNumbers(string line) {
        return ProcessLine(line[..1], line[1..], null, null);
    }

    static int ProcessLine(ReadOnlySpan<char> Head, ReadOnlySpan<char> Tail, int? first, int? last)
    {
        (first, last) = ProcessNewNumber(Head, first, last);
        return Tail.Length == 0
            ? first.Value * 10 + last.Value
            : ProcessLine(Tail[..1], Tail[1..], first, last);
    }

    return SplitLines(content)
        .Select(SumFirstAndLastNumbers)
        .Sum();
}

static int Second(string content) {
    var sum = 0;
    int? firstValue = null;
    int? lastValue = null;

    const int minLength = 3;
    const int maxLength = 5;

    int? ProcessChunk(ReadOnlySpan<char> chunk) {
        if (chunk.Length > 0 && char.IsDigit(chunk[0])) return null;
        return chunk.Length switch {
            < 3 => null,
            < 6 => Enum.TryParse<Numbers>(chunk.ToString(), out var value) ? (int)value : ProcessChunk(chunk.Slice(0, chunk.Length -1 )),
            _ => throw new InvalidOperationException("Chunk cannot be more than 5."),
        };
    }

    foreach (var line in content.Split('\n')) {
        if (line.Length == 0) continue;
        for (int index = 0; index < line.Length; ++index)
        {
            var length = Math.Min(line.Length - index, 5);
            var current = line[index];
            var newValue = char.IsDigit(current)
                ? int.Parse(current.ToString())
                : ProcessChunk(line.AsSpan(index, length));
            if (newValue.HasValue) {
                if (firstValue.HasValue) {
                    lastValue = newValue;
                } else {
                    firstValue = newValue;
                    lastValue = newValue;
                }
            }
        }
        (firstValue, lastValue) = Calculate(firstValue.Value, lastValue.Value, ref sum);
    }

    return sum;
}

static (int?, int?) Calculate(int first, int last, ref int acc) {
    acc += first * 10 + last;
    return (null, null);
}

enum Numbers {
    one = 1, two = 2, three = 3,
    four = 4, five = 5, six = 6,
    seven = 7, eight = 8, nine = 9
}
