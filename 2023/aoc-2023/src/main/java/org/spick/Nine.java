package org.spick;


import org.spick.utils.SlidingWindow;
import org.spick.utils.StreamUtils;

import java.util.List;
import java.util.function.LongBinaryOperator;
import java.util.function.ToLongFunction;

public class Nine extends AbstractPuzzle<Long> {
    public Nine(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Nine("nine.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    // 1930746032
    @Override
    public Long first() {
        return streamLines()
                .map(StreamUtils::parseListLong)
                .mapToLong(this::recurseFirst)
                .sum();
    }

    // 1154
    @Override
    public Long second() {
        return streamLines()
                .map(StreamUtils::parseListLong)
                .mapToLong(this::recurseSecond)
                .sum();
    }

    private long recurseFirst(List<Long> list) {
        return recurse(list, List::getLast, Long::sum);
    }

    private long recurseSecond(List<Long> list) {
        return recurse(list, List::getFirst, (a, b) -> a - b);
    }

    private Long recurse(List<Long> list, ToLongFunction<List<Long>> accessor, LongBinaryOperator operation) {
        // End condition: all have the same value
        if (list.stream().distinct().count() == 1) {
            return list.getFirst();
        }
        // otherwise: caluclate differences. Recurse.
        var differences = SlidingWindow.windowed(list, 2)
                .map(pair -> pair.reduce(0L, (a, b) -> b - a))
                .toList();
        if (differences.size() + 1 != list.size()) {
            panic("sizes not same");
        }
        // return last element of list + result of recursion
        return operation.applyAsLong(accessor.applyAsLong(list), recurse(differences, accessor, operation));
    }
}
