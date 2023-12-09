package org.spick;


import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Stream;

public class Nine extends AbstractPuzzle<Long> {
    public Nine(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Nine("nine.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Long first() {
        return streamLines()
                .map(parseList())
                .mapToLong(this::recurse)
                .sum();
    }

    private Long recurse(List<Long> list) {
        // End condition: all have the same value
        if (list.stream().distinct().count() == 1) {
            return list.getFirst();
        }
        // otherwise: caluclate differences. Recurse.
        var left = list.iterator();
        var right = list.iterator();
        right.next();
        var differences = new ArrayList<Long>();
        while (right.hasNext()) {
            differences.add(right.next() - left.next());
        }
        if (differences.size() + 1 != list.size()) {
            panic("sizes not same");
        }
        // return last element of list + result of recursion
        var result = list.getLast() + recurse(differences);
//        System.out.println(result);
        return result;
    }

    private Function<String, List<Long>> parseList() {
        return s -> Stream.of(s.split(" "))
                .mapToLong(Long::parseLong)
                .boxed()
                .toList();
    }

    @Override
    public Long second() {
        return null;
    }
}
