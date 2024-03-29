package org.spick;

import java.util.Map;
import java.util.function.Function;

import static java.util.function.Function.identity;

public class One extends AbstractPuzzle<Integer> {

    public static final Map<String, String> REPLACEMENTS = Map.of(
            "one", "1",
            "two", "2",
            "three", "3",
            "four", "4",
            "five", "5",
            "six", "6",
            "seven", "7",
            "eight", "8",
            "nine", "9");

    public One(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new One("one.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() {
        return streamLines()
                .map(x -> x.replaceAll("[^0-9]", ""))
                .map(x -> x.charAt(0) + x.substring(x.length() - 1))
                .mapToInt(Integer::parseInt)
                .sum();
    }

    @Override
    public Integer second() {
        return streamLines()
                .map(replacerFunction())
                .map(x -> x.replaceAll("[^0-9]", ""))
                .map(x -> x.charAt(0) + x.substring(x.length() - 1))
                .mapToInt(Integer::parseInt)
                .sum();
    }


    public Function<String, String> replacerFunction() {
        return REPLACEMENTS.entrySet().stream()
                .map(this::replace)
                .reduce(identity(), Function::compose);
    }

    /**
     * pumping lemma FTW. Well kinda, it's not exactly pumping lemma.
     * This just makes sure eightwone is replaced by both 8, 2 and 1 by realizing that
     * eightwone -> eightwo2twone is still parseable regardless of the order of encountering
     * the matches
     *
     * @param e
     * @return
     */
    private Function<String, String> replace(Map.Entry<String, String> e) {
        return s -> s.replaceAll(e.getKey(), e.getKey() + e.getValue() + e.getKey());
    }
}
