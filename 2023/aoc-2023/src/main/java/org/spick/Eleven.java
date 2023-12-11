package org.spick;

import java.util.ArrayList;
import java.util.Set;
import java.util.function.ToIntFunction;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Eleven extends AbstractPuzzle<Long> {

    public Eleven(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Eleven("eleven.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Long first() {
        return calculateDistances(2);
    }

    @Override
    public Long second() {
        return calculateDistances(1000000);
    }

    private static Set<Integer> getEmpty(ArrayList<Position> galaxies, ToIntFunction<Position> dimension, int length) {
        var galaxy = galaxies.stream()
                .mapToInt(dimension)
                .boxed()
                .collect(Collectors.toSet());
        var empty = IntStream.range(0, length)
                .filter(x -> !galaxy.contains(x))
                .boxed()
                .collect(Collectors.toSet());
        return empty;
    }

    private static ArrayList<Position> getGalaxies(String[][] matrix) {
        var galaxies = new ArrayList<Position>();
        for (int y = 0; y < matrix.length; y++) {
            for (int x = 0; x < matrix[0].length; x++) {
                if ("#".equals(matrix[y][x])) {
                    galaxies.add(new Position(x, y));
                }
            }
        }
        return galaxies;
    }

    private String[][] readMatrix() {
        return streamLines()
                .map(s -> s.split(""))
                .toArray(String[][]::new);
    }

    private long calculateDistances(final int ageOfUniverse) {
        var matrix = readMatrix();
        var galaxies = getGalaxies(matrix);
        var emptyColumns = getEmpty(galaxies, Position::x, matrix[0].length);
        var emptyRows = getEmpty(galaxies, Position::y, matrix.length);
        var sum = 0L;
        for (int i = 0; i < galaxies.size(); i++) {
            for (int j = i + 1; j < galaxies.size(); j++) {
                var first = galaxies.get(i);
                var second = galaxies.get(j);
                sum += getDistance(first.x(), second.x(), emptyColumns, ageOfUniverse - 1);
                sum += getDistance(first.y(), second.y(), emptyRows, ageOfUniverse - 1);
            }
        }
        return sum;
    }

    private static long getDistance(int first, int second, Set<Integer> empties, final int amountToFillIn) {
        var lower = Math.min(first, second);
        var upper = Math.max(first, second);
        var doubles = IntStream.range(lower, upper)
                .filter(empties::contains)
                .count();
        return upper - lower + (doubles * amountToFillIn);
    }

    public record Position(int x, int y) {
    }
}
