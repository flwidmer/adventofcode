package org.spick;

import java.util.Arrays;
import java.util.List;

public class Six extends AbstractPuzzle<Long> {

    public static final String SPLIT_BY_SPACE = "[^0-9 ]+";
    public static final String IGNORE_SPACE = "[^0-9]+";

    public Six(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Six("six.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.firstBruteForce());
        System.out.println(puzzle.second());
        System.out.println(puzzle.secondBruteForce());
    }

    public Long firstBruteForce() {
        var lists = readInput(SPLIT_BY_SPACE);
        return calculateBruteFroce(lists);
    }

    public Long secondBruteForce() {
        var lists = readInput(IGNORE_SPACE);
        return calculateBruteFroce(lists);
    }

    @Override
    public Long first() {
        var lists = readInput(SPLIT_BY_SPACE);
        return calculateDirectly(lists);
    }

    @Override
    public Long second() {
        var lists = readInput(IGNORE_SPACE);
        return calculateDirectly(lists);
    }

    /**
     * solving equation for binomial
     *
     * @param lists
     * @return
     */
    private Long calculateDirectly(List<List<Long>> lists) {
        var times = lists.get(0);
        var distances = lists.get(1);
        var winProduct = 1L;
        for (int race = 0; race < times.size(); race++) {
            var winCount = 0L;
            var totalTime = times.get(race);
            var distanceToBeat = distances.get(race);
            var x1 = (-(Math.sqrt(Math.pow(totalTime, 2) - 4 * distanceToBeat) - totalTime)) / 2;
            var x2 = (Math.sqrt(Math.pow(totalTime, 2) - 4 * distanceToBeat) + totalTime) / 2;
            winProduct = (long) (winProduct * (x2 -x1));
        }
        return winProduct;
    }

    private List<List<Long>> readInput(String regex) {
        return streamLines()
                .map(s -> s.replaceAll(regex, ""))
                .map(this::parseList)
                .toList();
    }

    private static long calculateBruteFroce(List<List<Long>> lists) {
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
