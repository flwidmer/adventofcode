package nine;

import eight.Eight;
import util.Pair;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Nine {
    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        //        second("input.txt");
    }

    private static void first(String fileName) throws URISyntaxException, IOException {
        final Integer[][] matrix = parseInput(fileName);
        var xLength = matrix[0].length;
        var yLength = matrix.length;
        List<Integer> localMinima = new ArrayList<>();
        for (int i = 0; i < yLength; i++) {
            for (int k = 0; k < xLength; k++) {
                if (check(matrix, k, i, -1, -1)
                        && check(matrix, k, i, -1, 0)
                        && check(matrix, k, i, -1, 1)
                        && check(matrix, k, i, 0, -1)
                        //                        || check(matrix, i,k,0,0)
                        && check(matrix, k, i, 0, 1)
                        && check(matrix, k, i, 1, -1)
                        && check(matrix, k, i, 1, 0)
                        && check(matrix, k, i, 1, 1)
                ) {
                    localMinima.add(matrix[i][k]);
                }
            }
        }
        final int sum = localMinima.stream()
                .mapToInt(i -> i)
                .map(i -> i + 1)
                .sum();
        System.out.println(sum);
    }

    private static void second(String fileName) throws URISyntaxException, IOException {
        final Integer[][] matrix = parseInput(fileName);
        var xLength = matrix[0].length;
        var yLength = matrix.length;
        var basinNumber = -1;
        Map<Integer, Set<Integer>> unionFind = new HashMap<>();
        for (int i = 0; i < yLength; i++) {
            for (int k = 0; k < xLength; k++) {
                if (matrix[i][k] == 9) {
                    continue;
                }
                var found = false;
                var nw = checkBasin(matrix, k, i, -1, -1);
                if (nw < 0) {
                    if (found) {

                    } else {
                        matrix[i][k] = nw;
                        found = true;
                    }
                }
                var n = checkBasin(matrix, k, i, 0, -1);
                if (n < 0) {
                    if (found) {

                    } else {
                        matrix[i][k] = n;
                        found = true;
                    }
                }
                var w = checkBasin(matrix, k, i, -1, 0);
                if (w < 0) {
                    if (found) {

                    } else {
                        matrix[i][k] = w;
                        found = true;
                    }
                }
                if(!found) {
                    matrix[i][k] = basinNumber;
                    unionFind.put(basinNumber, Set.of(basinNumber));
                    basinNumber--;
                }
            }
        }
    }

    private static Integer checkBasin(final Integer[][] matrix, final int x, final int y, final int moveX, final int moveY) {
        var xLength = matrix[0].length;
        var yLength = matrix.length;
        if (x + moveX < 0 || y + moveY < 0 || x + moveX >= xLength || y + moveY >= yLength) {
            return 1;
        }
        var value = matrix[y + moveY][x + moveX];
        if (value < 0) {
            return value;
        } else if (value == 9) {
            return 1;
        } else {
            throw new IllegalArgumentException("Should not happen");
        }
    }

    private static boolean check(final Integer[][] matrix, final int x, final int y, final int moveX, final int moveY) {
        var xLength = matrix[0].length;
        var yLength = matrix.length;
        if (x + moveX < 0 || y + moveY < 0 || x + moveX >= xLength || y + moveY >= yLength) {
            return true;
        }
//        return matrix[y][x] <;
        //XXX unfinished
        return false;
    }

    private static Integer[][] parseInput(final String fileName) throws IOException, URISyntaxException {
        URL resource = Nine.class.getResource(fileName);
        assert resource != null;
        var listMatrix = Files.lines(Paths.get(resource.toURI()))
                .map(s -> s.split(""))
                .map(l -> Arrays.stream(l)
                        .map(Integer::parseInt)
                        .collect(Collectors.toList()))
                .collect(Collectors.toList());
        var y = listMatrix.size();
        var x = listMatrix.get(0).size();
        Integer[][] result = new Integer[y][x];
        var xCount = 0;
        var yCount = 0;
        for (List<Integer> line : listMatrix) {
            for (Integer number : line) {
                result[yCount][xCount++] = number;
            }
            xCount = 0;
            yCount++;
        }
        return result;
    }
}
