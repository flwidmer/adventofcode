package org.spick;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.spick.utils.StreamUtils.collectEntriesIntoMap;

public class Two extends AbstractPuzzle<Integer> {

    private static final Map<String, Integer> COLORS = Map.of(
            "red", 12,
            "green", 13,
            "blue", 14
    );

    public Two(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Two("two.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() {
        return streamLines()
                .map(this::parseGame)
                .map(Game::findMaxima)
                .filter(this::filterConditionFirst)
                .mapToInt(Maxima::id)
                .sum();
    }

    @Override
    public Integer second() {
        return streamLines()
                .map(this::parseGame)
                .map(Game::findMaxima)
                .mapToInt(Maxima::power)
                .sum();
    }

    private Game parseGame(String line) {
        var colon = line.split(":");
        var gameId = Integer.parseInt(colon[0].replaceAll("[^0-9]", ""));
        var commaSemicolon = colon[1].split("[,;]");
        var pattern = Pattern.compile("(\\d+)\\s([A-z]+)");
        var colorList = Stream.of(commaSemicolon)
                .map(String::trim)
                .map(pattern::matcher)
                .filter(Matcher::matches)
                .map(matcher -> {
                    var num = Integer.parseInt(matcher.group(1));
                    var name = matcher.group(2);
                    return new Color(name, num);
                })
                .collect(Collectors.toList());
        return new Game(gameId, colorList);
    }

    private boolean filterConditionFirst(Maxima maxima) {
        return COLORS.entrySet().stream()
                .map(e -> maxima.colors().get(e.getKey()) <= e.getValue())
                .reduce(true, (l, r) -> l && r);
    }

    public record Game(int id, List<Color> colors) {

        public Maxima findMaxima() {
            var maxima = colors().stream()
                    .map(Color::name)
                    // use grouping collector or something
                    .map(color -> Map.entry(color, findMaxForColor(color)))
                    .collect(collectEntriesIntoMap());
            return new Maxima(id(), maxima);
        }

        private int findMaxForColor(String color) {
            return colors().stream()
                    .filter(c -> c.name().equals(color))
                    .mapToInt(Color::amount)
                    .max()
                    .orElse(0);
        }
    }

    public record Maxima(int id, Map<String, Integer> colors) {
        public int power() {
            return colors().values().stream().mapToInt(i -> i).reduce(1, (a, b) -> a * b);
        }
    }

    public record Color(String name, int amount) {
    }
}
