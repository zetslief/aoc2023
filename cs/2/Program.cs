const string DataFile = "./../../data/2.txt";

const int maxRed = 12;
const int maxGreen = 13;
const int maxBlue = 14;

var fileContent = File.ReadAllText(DataFile); 
Console.WriteLine(fileContent);
var games = new List<Game>();
foreach (var line in fileContent.Split(Environment.NewLine))
{
    if (string.IsNullOrEmpty(line)) break;
    var game = ParseGame(line);
    games.Add(game);
}

Console.WriteLine($"First: {First(games)}");
Console.WriteLine($"First: {Second(games)}");

static int First(IEnumerable<Game> games) => games
    .Where(game => game.Sets.All(set => set.Red <= maxRed && set.Green <= maxGreen && set.Blue <= maxBlue))
    .Select(game => game.Id)
    .Sum();

static int Second(IEnumerable<Game> games)
{
    static IEnumerable<int> GetMinNumberOfColors(IEnumerable<Set> sets)
    {
        yield return sets.Select(s => s.Red).Max();
        yield return sets.Select(s => s.Green).Max();
        yield return sets.Select(s => s.Blue).Max();
    }

    static int Power(IEnumerable<int> colors)
        => colors.Aggregate((left, right) => left * right);
    
    return games.Select(game => game.Sets)
        .Select(GetMinNumberOfColors)
        .Select(Power)
        .Sum();
}



static Game ParseGame(string line)
{
    static int? ParseGameId(ref ReadOnlySpan<char> slice)
    {
        var end = slice.IndexOf(':');
        if (end < 0) return null;
        if (int.TryParse(slice[..end], out var gameId))
        {
            slice = slice[end..];
            return gameId;
        }
        else
        {
            return null;
        }
    }

    static IReadOnlyCollection<Set>? ParseAllSets(ref ReadOnlySpan<char> slice)
    {
        var result = new List<Set>();
        slice = slice.TrimStart(':');
        while (slice.Length > 0)
        {
            slice = slice.TrimStart(';');
            var set = ParseSet(ref slice);
            if (set is null) return null;
            result.Add(set);
        }
        return result;
    }

    static Set? ParseSet(ref ReadOnlySpan<char> slice)
    {
        int red = 0, green = 0, blue = 0;
        var setEnd = slice.IndexOf(';');
        if (setEnd < 0) setEnd = slice.Length;
        var setSlice = slice[..setEnd];
        slice = slice[setEnd..];
        while (setSlice.Length > 0)
        {
            setSlice = setSlice.TrimStart(','); // not-first colors are separated by commas.
            setSlice = setSlice.TrimStart(); // there is allways space at the beginning.
            var colorNumber = ParseColorNumber(ref setSlice);
            if (colorNumber is null) return null;
            setSlice = setSlice.TrimStart(); // there is allways space after color.
            var color = ParseColor(ref setSlice);
            if (color is null) return null;
            switch (color)
            {
                case Color.red:
                    red += colorNumber.Value;
                    break;
                case Color.green:
                    green += colorNumber.Value;
                    break;
                case Color.blue:
                    blue += colorNumber.Value;
                    break;
                default:
                    throw new NotSupportedException($"Not supported: {color}");
            }
        }
        return new Set(red, green, blue);
    }

    static Color? ParseColor(ref ReadOnlySpan<char> slice)
    {
        var colorEnd = slice.IndexOf(',');
        if (colorEnd < 0) colorEnd = slice.Length;
        if (Enum.TryParse<Color>(slice[..colorEnd], out var color))
        {
            slice = slice[colorEnd..];
            return color;
        }
        else return null;
    }

    static int? ParseColorNumber(ref ReadOnlySpan<char> slice)
    {
        var numberEnd = slice.IndexOf(' ');
        if (numberEnd < 0) return null;
        if (int.TryParse(slice[..numberEnd], out int colorNumber))
        {
            slice = slice[numberEnd..];
            return colorNumber;
        }
        else return null;
    }

    var span = line.AsSpan()[4..];
    return new Game(
        ParseGameId(ref span) ?? throw new ArgumentException($"Failed to parse game id in {line}"),
        ParseAllSets(ref span) ?? throw new ArgumentException($"Failed to parse sets in {line}")
    );
}

record Game(int Id, IReadOnlyCollection<Set> Sets);
record Set(int Red, int Green, int Blue);

enum Color { red, green, blue };