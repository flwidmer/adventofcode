package five;

import util.Pair;
import util.StringUtil;

import java.awt.*;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.OptionalInt;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.IntSupplier;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Five {

    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        second("input.txt");
    }

    private static void first(String fileName) throws URISyntaxException, IOException {
        URL resource = Five.class.getResource(fileName);
        List<Pair<Point, Point>> parsedInput = parseInput(resource);
        //determine matrix size
        Pair<Integer, Integer> maxSize = calculateMaxSize(parsedInput);

        Integer[][] matrix =  initializeMatrix(maxSize);

        parsedInput.stream()
                //Filter diagonal
                .filter(diagonalPredicate())
                .map(canonicalOrder())
                //map to points
                .map(calculateTouchedPoints())
                .flatMap(List::stream)
                // update points
                .forEach(updatePoints(matrix));
        // find counts
        final long count = getCount(matrix);
        System.out.println(count);
    }

    private static long getCount(final Integer[][] matrix) {
        return Arrays.stream(matrix)
                .flatMap(Arrays::stream)
                .filter(x -> x >= 2)
                .count();
    }

    private static List<Pair<Point, Point>> parseInput(final URL resource) throws IOException, URISyntaxException {
        return Files.lines(Paths.get(resource.toURI()))
                .map(StringUtil.split(" -> "))
                .map(parsePoints())
                .collect(Collectors.toList());
    }

    private static void second(String fileName) throws URISyntaxException, IOException {
        URL resource = Five.class.getResource(fileName);
        List<Pair<Point, Point>> parsedInput = parseInput(resource);
        //determine matrix size
        Pair<Integer, Integer> maxSize = calculateMaxSize(parsedInput);
        Integer[][] matrix = initializeMatrix(maxSize);
        parsedInput.stream()
                .map(canonicalOrder())
                //map to points
                .map(calculateTouchedPoints())
                .flatMap(List::stream)
                // update points
                .forEach(updatePoints(matrix));
        // find counts
        final long count = getCount(matrix);
        System.out.println(count);
    }

    private static Integer[][] initializeMatrix(final Pair<Integer, Integer> maxSize) {
        Integer[][] matrix = new Integer[maxSize.getL()][maxSize.getR()];
        for (int i = 0; i < maxSize.getL(); i++) {
            for (int k = 0; k < maxSize.getR(); k++) {
                matrix[i][k] = 0;
            }
        }
        return matrix;
    }

    private static Function<Pair<Point, Point>, Pair<Point, Point>> canonicalOrder() {
        return pp -> {
            var p2 = pp.getR();
            var p1 = pp.getL();

            var x1 = p1.getL();
            var x2 = p2.getL();
            var y1 = p1.getR();
            var y2 = p2.getR();

            if(x1*x1 + y1*y1 < x2*x2+y2*y2) {
                return new Pair<>(p1, p2);
            } else {
                return new Pair<>(p2,p1);
            }
        };
    }

    private static Consumer<Point> updatePoints(final Integer[][] matrix) {
        return p -> {
            var current = matrix[p.getL()][p.getR()];
            matrix[p.getL()][p.getR()] = current + 1;
        };
    }

    private static Function<Pair<Point, Point>, List<Point>> calculateTouchedPoints() {
        return t -> {
            var p2 = t.getR();
            var p1 = t.getL();

            var x1 = p1.getL();
            var x2 = p2.getL();
            var y1 = p1.getR();
            var y2 = p2.getR();

            var steps = Integer.max(x2 - x1, y2 - y1) + 1;
            var xSteps = stepGenerator(x1, x2, steps);
            var ySteps = stepGenerator(y1, y2, steps);
            return IntStream.range(0, steps)
                    .mapToObj(x -> new Point(xSteps.getAsInt(), ySteps.getAsInt()))
                    .collect(Collectors.toList());
        };
    }

    public static IntSupplier stepGenerator(Integer from, Integer to, Integer steps) {
        return new IntSupplier() {

            Integer current = from;
            Integer increment = from < to?1:-1;

            @Override
            public int getAsInt() {
                if(from.equals(to)) {
                    return from;
                }
                var toReturn = current;
                if (!current.equals(to)) {
                    current+=increment;
                }
                return toReturn;
            }
        };
    }

    private static Predicate<? super Pair<Point, Point>> diagonalPredicate() {
        return t -> {
            var p2 = t.getR();
            var p1 = t.getL();

            var x1 = p1.getL();
            var x2 = p2.getL();
            var y1 = p1.getR();
            var y2 = p2.getR();

            return x1.equals(x2) || y1.equals(y2);
        };
    }

    private static Pair<Integer, Integer> calculateMaxSize(final List<Pair<Point, Point>> parsedInput) {
        final OptionalInt xMax = parsedInput.stream()
                .flatMap(p -> Stream.of(p.getL(), p.getR()))
                .mapToInt(Pair::getL)
                .max();
        final OptionalInt yMax = parsedInput.stream()
                .flatMap(p -> Stream.of(p.getL(), p.getR()))
                .mapToInt(Pair::getR)
                .max();
        return new Pair<>(xMax.orElse(0) + 1, yMax.orElse(0) + 1);
    }

    public static Function<Stream<String>, Pair<Point, Point>> parsePoints() {
        return s -> {
            final List<Point> collect =
                    s.map(ss -> StringUtil.split(",")
                                    .apply(ss)
                                    .map(Integer::parseInt)
                                    .collect(Collectors.toList()))
                            .map(ss -> new Point(ss.get(0), ss.get(1)))
                            .collect(Collectors.toList());
            return new Pair<>(collect.get(0), collect.get(1));
        };
    }

    public static class Point extends Pair<Integer, Integer> {
        public Point(final Integer integer, final Integer integer2) {
            super(integer, integer2);
        }
    }
}
