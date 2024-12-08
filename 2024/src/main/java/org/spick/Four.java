package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

import org.spick.utils.Pair;

public class Four extends AbstractPuzzle<Integer> {

    public Four(String input) {
        super(input);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        Four four = new Four("four.txt");
        System.out.println(four.first());
        System.out.println(four.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        var matrix = getInput();
        var counter = 0;
        var directions = List.of(
                new Pair<>(-1, -1),
                new Pair<>(-1, 1),
                new Pair<>(1, -1),
                new Pair<>(1, 1),
                new Pair<>(-1, 0),
                new Pair<>(0, -1),
                new Pair<>(1, 0),
                new Pair<>(0, 1));
        for (int i = 0; i < matrix.size(); i++) {
            for (int k = 0; k < matrix.get(i).size(); k++) {
                for (var direction : directions) {
                    if (check(matrix, i, k, direction, List.of("X", "M", "A", "S").iterator())) {
                        counter++;
                    }
                }
            }
        }
        return counter;
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var matrix = getInput();
        var counter = 0;
        var directions = List.of(
                new Pair<>(-1, -1),
                new Pair<>(-1, 1),
                new Pair<>(1, -1),
                new Pair<>(1, 1));
        for (int i = 0; i < matrix.size(); i++) {
            for (int k = 0; k < matrix.get(i).size(); k++) {
                var cross = 0;
                // A is the center
                if (matrix.get(i).get(k).equals("A")) {
                    for (var direction : directions) {
                        // one step in the direction
                        var nextI = i + direction.getLeft();
                        var nextK = k + direction.getRight();
                        // check back
                        if (check(matrix, nextI, nextK, invert(direction), List.of("M", "A", "S").iterator())) {
                            cross++;
                        }
                    }
                    if (cross == 2) {
                        counter++;
                    }
                }
            }
        }
        return counter;
    }

    private Pair<Integer, Integer> invert(Pair<Integer, Integer> direction) {
        return new Pair<>(direction.getLeft() * -1, direction.getRight() * -1);
    }

    /**
     * recursively check the matrix
     * @param matrix the matrix
     * @param i x position
     * @param k y position
     * @param direction offsets added to position
     * @param iterator what we are looking for
     * @return
     */
    private boolean check(List<List<String>> matrix, int i, int k, Pair<Integer, Integer> direction, Iterator<String> iterator) {
        if (!isSafeMove(matrix, i, k)) {
            return false;
        }
        if (matrix.get(i).get(k).equals(iterator.next())) {
            if (!iterator.hasNext()) {
                return true;
            }
            var nextI = i + direction.getLeft();
            var nextK = k + direction.getRight();
            return check(matrix, nextI, nextK, direction, iterator);
        } else {
            return false;
        }
    }

    private boolean isSafeMove(List<List<String>> matrix, int nextI, int nextK) {
        if (nextI < 0 || nextI >= matrix.size()) {
            return false;
        }
        if (nextK < 0 || nextK >= matrix.get(nextI).size()) {
            return false;
        }
        return true;
    }

    private List<List<String>> getInput() {
        return streamLines()
                .map(line -> Arrays.asList(line.split("")))
                .toList();
    }

}
