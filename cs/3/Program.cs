var fileContent = File
    .ReadAllText("./../../data/3.txt")
    .TrimEnd();

Console.WriteLine(fileContent);

var fileSpan = fileContent.AsSpan();

var width = fileSpan.IndexOf('\n') + 1;
if (width < 0) throw new InvalidDataException("Width cannot be < 0.");
Console.WriteLine($"Width: {width}");

var numbers = new List<Number>();
var symbols = new List<Symbol>();
var cursor = 0;

while (fileSpan.Length > cursor)
{
    switch (fileSpan[cursor])
    {
        case '.':
        case '\n':
            ++cursor; 
            break;
        case var digit when char.IsDigit(digit):
            var number = ParseNumber(in fileSpan, ref cursor)
                ?? throw new InvalidOperationException("Failed to parse number");
            numbers.Add(number);
            break; 
        case var @char:
            symbols.Add(new Symbol(cursor, @char));
            ++cursor; 
            break;
        default:
            throw new NotImplementedException($"Unknown char: {fileSpan[cursor]}");
    }
}

var connectedNumbers = 
    numbers.Where(n => IsNumberConnected(n, symbols, width))
    .ToArray();

var first = connectedNumbers
    .Select(n => n.Value)
    .Sum();

Console.WriteLine($"First: {first}");

var gearRatioSum = symbols
    .Where(s => s.Char == '*')
    .Select(symbol => {
        var nums = GetConnectedNumbers(symbol, connectedNumbers, width).ToArray();
        return nums.Length == 2
            ? nums[0].Value * nums[1].Value
            : 0;
    })
    .Sum();

Console.WriteLine($"Second: {gearRatioSum}");


static bool IsNumberConnected(Number number, IEnumerable<Symbol> symbols, int width)
{
    foreach (var symbol in symbols)
    {
        var (symbolRow, symbolColumn) = FromFlatIndex(symbol.Index, width); 
        var (numberStartRow, numberStartColumn) = FromFlatIndex(number.Start, width);
        var (_, numberEndColumn) = FromFlatIndex(number.End, width);
        if (Math.Abs(symbolRow - numberStartRow) > 1) continue;
        numberStartColumn = Math.Max(0, numberStartColumn - 1); 
        numberEndColumn = Math.Min(width - 1, numberEndColumn);
        if (symbolColumn < numberStartColumn || symbolColumn > numberEndColumn + 1) continue;
        return true;
    }
    return false;
}

static IEnumerable<Number> GetConnectedNumbers(Symbol symbol, IEnumerable<Number> numbers, int width)
{
    return numbers.Where(number => IsNumberConnected(number, Enumerable.Repeat(symbol, 1), width));
}

static (int Row, int Column) FromFlatIndex(int index, int width)
    => (index / width, index % width);

static Number? ParseNumber(in ReadOnlySpan<char> span, ref int cursor)
{
    var start = cursor;
    for (; cursor < span.Length; ++cursor)
    {
        if (char.IsDigit(span[cursor])) continue;
        else
        {
            if (int.TryParse(span[start..cursor], out var number))
            {
                return new Number(start, cursor - 1, number);
            }
            else break;
        }
    }

    return null;
}

record Number(int Start, int End, int Value); 
record Symbol(int Index, char Char);