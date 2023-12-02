package org.spick;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Two extends AbstractPuzzle<Integer> {

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
                .map(this::findMaxima)
                .filter(this::filterConditionFirst)
                .mapToInt(Maxima::id)
                .sum();
    }

    @Override
    public Integer second() {
        return streamLines()
                .map(this::parseGame)
                .map(this::findMaxima)
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

    private Maxima findMaxima(Game fullGame) {
        var result = new HashMap<String, Integer>();
        for (Color c : fullGame.colors) {
            result.computeIfPresent(c.name, (key, old) -> old < c.amount() ? Integer.valueOf(c.amount()) : old);
            result.computeIfAbsent(c.name(), k -> c.amount());
        }
        return new Maxima(fullGame.id(), result);
    }

    private boolean filterConditionFirst(Maxima maxima) {
        var red = maxima.colors().get("red") <= 12;
        var green = maxima.colors().get("green") <= 13;
        var blue = maxima.colors().get("blue") <= 14;
        return red && green && blue;
    }

    public record Game(int id, List<Color> colors) {
    }

    public record Maxima(int id, Map<String, Integer> colors) {
        public int power() {
            return colors().values().stream().mapToInt(i -> i).reduce(1, (a, b) -> a * b);
        }
    }

    public record Color(String name, int amount) {
    }
}
