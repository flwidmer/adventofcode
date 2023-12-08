package org.spick;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Four extends AbstractPuzzle<Integer> {

    public Four(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Four("four.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() {
        return streamLines().map(this::parseLine)
                .mapToInt(this::countPointsFirst)
                .sum();
    }

    private Integer countPointsFirst(ParsedLine parsedLine) {
        var count = parsedLine.winnerCount();
        return 1 << count - 1;
    }

    private ParsedLine parseLine(String string) {
        var colon = string.split(":");
        var gameId = Integer.parseInt(colon[0].replaceAll("[^0-9]+", ""));
        var pipe = colon[1].split("\\|");
        var winners = parseList(pipe[0]);
        var mine = parseList(pipe[1]);
        return new ParsedLine(gameId, winners, mine, getWinnerCount(mine, winners));
    }

    private int getWinnerCount(List<Integer> mine, List<Integer> winners) {
        var mineSet = new HashSet<>(mine);
        var count = winners.stream()
                .filter(mineSet::contains)
                .count();
        return (int) count;
    }

    private List<Integer> parseList(String list) {
        var space = list.split(" ");
        return Arrays.stream(space)
                .filter(s -> !s.isBlank())
                .map(Integer::parseInt)
                .toList();
    }

    @Override
    public Integer second() {
        var cards = streamLines()
                .map(this::parseLine)
                .collect(Collectors.toMap(ParsedLine::number, Function.identity()));
        var winCounter = 0;
        var map = new TreeMap<Integer, Integer>();
        cards.forEach((key, _) -> map.put(key, 1));
        while (!map.isEmpty()) {
            winCounter++;
            var pop = cards.get(pop(map));
            var winnerCount = pop.winnerCount();
            // cache update? as function mabe?
            IntStream.range(0, winnerCount)
                    .map(i -> i + pop.number + 1)
                    .mapToObj(cards::get)
                    .forEach(w -> {
                        changeCounter(w.number(), 1, map);
                    });
        }
        return winCounter;
    }

    private static void changeCounter(int key, final int by, TreeMap<Integer, Integer> map) {
        map.merge(key, by, Integer::sum);
    }

    private static Integer pop(TreeMap<Integer, Integer> map) {
        var pop = map.firstEntry().getKey();
        changeCounter(pop, -1, map);
        // remove if list is empty
        if (map.firstEntry().getValue() == 0) {
            map.remove(map.firstKey());
        }
        return pop;
    }

    private record ParsedLine(int number, List<Integer> winners, List<Integer> mine, int winnerCount) {
    }
}
