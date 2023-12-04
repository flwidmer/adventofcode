package org.spick;

import java.util.ArrayList;
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

    /**
     * Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
     * Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
     * Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
     * Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
     * Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
     * Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
     *
     * @return
     */

    @Override
    public Integer first() {
        return streamLines().map(this::parseLine)
                .mapToInt(this::countPoinsFirst)
                .sum();
    }

    private Integer countPoinsFirst(ParsedLine parsedLine) {
        var count = parsedLine.winnerCount();
        var points = 1;
        points = points << count - 1;
        return points;
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
        var map = new TreeMap<Integer, List<ParsedLine>>();
        cards.entrySet()
                .forEach(e -> map.put(e.getKey(), new ArrayList<>(List.of(e.getValue()))));
        while (!map.isEmpty()) {
            winCounter++;
            var pop = pop(map);
            var winnerCount = pop.winnerCount();
            IntStream.range(0, winnerCount)
                    .map(i -> i + pop.number + 1)
                    .mapToObj(cards::get)
                    .forEach(w -> {
                        map.get(w.number()).add(w);
                    });
        }
        return winCounter;
    }

    private static ParsedLine pop(TreeMap<Integer, List<ParsedLine>> map) {
        var pop = map.firstEntry().getValue().removeFirst();
        // remove if list is empty
        if (map.firstEntry().getValue().isEmpty()) {
            map.remove(map.firstKey());
        }
        return pop;
    }

    private record ParsedLine(int number, List<Integer> winners, List<Integer> mine, int winnerCount) {
    }
}
