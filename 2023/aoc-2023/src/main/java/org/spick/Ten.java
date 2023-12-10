package org.spick;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.function.Supplier;
import java.util.function.ToIntFunction;

import static java.lang.Math.max;

public class Ten extends AbstractPuzzle<Integer> {
    public Ten(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Ten("ten.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() {
        return 0;
    }

    public Integer firstBfs() {
        var matrix = parseMatrix();
        // find starting position
        var startPosition = findStartPosition(matrix);
        return bfs(matrix, startPosition, 0, Set.of()) / 2;
    }

    /**
     * Recursion based, probably need a bit bigger stack. -Xss1G
     * <p>
     * | is a vertical pipe connecting north and south.
     * - is a horizontal pipe connecting east and west.
     * L is a 90-degree bend connecting north and east.
     * J is a 90-degree bend connecting north and west.
     * 7 is a 90-degree bend connecting south and west.
     * F is a 90-degree bend connecting south and east.
     * . is ground; there is no pipe in this tile.
     * S is the starting position of the animal; there is a pipe on this
     *
     * @param matrix
     * @param position
     * @param currentCount
     * @param encounteredPositions
     * @return
     */
    private Integer bfs(String[][] matrix, Position position, int currentCount, Set<Position> encounteredPositions) {
        if (encounteredPositions.size() > 1 && "S".equals(position.getCharacterAt(matrix))) {
            // we're finished.
            return currentCount;
        }
        if (encounteredPositions.contains(position)) {
            return -2;
        }
        var history = new LinkedHashSet<>(encounteredPositions);
        history.add(position);
        ToIntFunction<Supplier<Position>> go = move -> bfs(matrix, move.get(), currentCount + 1, history);
        return switch (position.getCharacterAt(matrix)) {
            case "|" -> max(go.applyAsInt(position::north), go.applyAsInt(position::south));
            case "-" -> max(go.applyAsInt(position::east), go.applyAsInt(position::west));
            case "L" -> max(go.applyAsInt(position::north), go.applyAsInt(position::east));
            case "J" -> max(go.applyAsInt(position::north), go.applyAsInt(position::west));
            case "7" -> max(go.applyAsInt(position::south), go.applyAsInt(position::west));
            case "F" -> max(go.applyAsInt(position::south), go.applyAsInt(position::east));
            case "." -> -1;
            case "S" -> max(go.applyAsInt(position::north), max(go.applyAsInt(position::south),
                    max(go.applyAsInt(position::east), go.applyAsInt(position::west))));
            default -> panic("unknown character encountered");
        };
    }

    @Override
    public Integer second() {
        return null;
    }

    private String[][] parseMatrix() {
        return streamLines()
                .map(s -> s.split(""))
                .toArray(String[][]::new);
    }

    private static Position findStartPosition(String[][] matrix) {
        for (int y = 0; y < matrix.length; y++) {
            for (int x = 0; x < matrix[0].length; x++) {
                if ("S".equals(matrix[y][x])) {
                    System.out.println("start found: " + y + " " + x);
                    return new Position(x, y);
                }
            }
        }
        return panic("no start found");
    }

    public record Position(int x, int y) {
        public String getCharacterAt(String[][] matrix) {
            if (x < 0 || x >= matrix.length || y < 0 || y >= matrix.length) {
                return "."; // out of bounds
            }
            return matrix[y()][x()];
        }

        // keep in mind the slightly inverted nature of this coordinate system
        public Position north() {
            return new Position(x, y - 1);
        }

        public Position south() {
            return new Position(x, y + 1);
        }

        public Position east() {
            return new Position(x + 1, y);
        }

        public Position west() {
            return new Position(x - 1, y);
        }
    }
}
