package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.List;
import java.util.function.LongBinaryOperator;
import java.util.stream.Stream;

import org.spick.utils.Pair;

public class Seven extends AbstractPuzzle<Long> {

    private static Operator PLUS = new Operator((l, r) -> l + r, 0L);
    private static Operator MULTIPLY = new Operator((l, r) -> l * r, 1L);
    private static Operator CONCAT = new Operator((l, r) -> Long.parseLong(Long.toString(l) + Long.toString(r)), 0L);

    public Seven(String input) {
        super(input);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        var puzzle = new Seven("seven.txt");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Long first() throws IOException, URISyntaxException {
        return getInput()
        .mapToLong(line -> check(line, List.of(PLUS, MULTIPLY)))
                .sum();
    }

    @Override
    public Long second() throws IOException, URISyntaxException {
        return getInput()
        .mapToLong(line -> check(line, List.of(PLUS, MULTIPLY, CONCAT)))
                .sum();
    }

    private Long check(Line line, List<Operator> operators) {
        for (var nextOp : operators) {
            if (find(line.target(), nextOp, nextOp.identity(), line.values(), operators)) {
                return line.target();
            }
        }
        return 0L;
    }

    private boolean find(Long target, Operator op, Long current, List<Long> remaining,
            List<Operator> operations) {
        if (remaining.isEmpty()) {
            return target.equals(current);
        }
        var nextCurrent = op.application().applyAsLong(current, remaining.getFirst());
        if (nextCurrent > target) {
            return false;
        }
        var subList = remaining.subList(1, remaining.size());
        for (var nextOp : operations) {
            if (find(target, nextOp, nextCurrent, subList, operations)) {
                return true;
            }
        }
        return false;
    }

    private Stream<Line> getInput() {
        return streamLines()
                .map(line -> line.split(":"))
                .map(splitLine -> new Pair<>(Long.parseLong(splitLine[0]), splitLine[1]))
                .map(p -> p.mapRight(s -> s.split(" ")))
                .map(p -> p.mapRight(a -> Arrays.stream(a)
                        .filter(s -> !s.isEmpty())
                        .map(Long::parseLong)
                        .toList()))
                .map(p -> new Line(p.getLeft(), p.getRight()));
    }

    private record Line(Long target, List<Long> values) {
    }

    private record Operator(LongBinaryOperator application, Long identity) {
    }
}
