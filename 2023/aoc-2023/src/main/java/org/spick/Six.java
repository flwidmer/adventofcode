package org.spick;

import java.util.Arrays;
import java.util.List;

public class Six extends AbstractPuzzle<Long> {
    public Six(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Six("six.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Long first() {
        var lists = streamLines()
                .map(s -> s.replaceAll("[^0-9 ]+", ""))
                .map(this::parseList)
                .toList();
        var times = lists.get(0);
        var distances = lists.get(1);
        var winProduct = 1L;
        for (int race = 0; race < times.size(); race++) {
            var winCount = 0L;
            var totalTime = times.get(race);
            var distanceToBeat = distances.get(race);
            for (int selectedSpeeed = 0; selectedSpeeed < totalTime; selectedSpeeed++) {
                if (selectedSpeeed * (totalTime - selectedSpeeed) > distanceToBeat) {
                    winCount++;
                } else if (winCount > 0) {
                    break;
                }
            }
            winProduct = winProduct * winCount;
        }
        return winProduct;
    }

    @Override
    public Long second() {
        var lists = streamLines()
                .map(s -> s.replaceAll("[^0-9]+", ""))
                .map(this::parseList)
                .toList();
        var times = lists.get(0);
        var distances = lists.get(1);
        var winProduct = 1L;
        for (int race = 0; race < times.size(); race++) {
            var winCount = 0L;
            var totalTime = times.get(race);
            var distanceToBeat = distances.get(race);
            for (int selectedSpeeed = 0; selectedSpeeed < totalTime; selectedSpeeed++) {
                if (selectedSpeeed * (totalTime - selectedSpeeed) > distanceToBeat) {
                    winCount++;
                } else if (winCount > 0) {
                    break;
                }
            }
            winProduct = winProduct * winCount;
        }
        return winProduct;
    }

    private List<Long> parseList(String list) {
        var space = list.split(" ");
        return Arrays.stream(space)
                .filter(s -> !s.isBlank())
                .map(Long::parseLong)
                .toList();
    }
}
