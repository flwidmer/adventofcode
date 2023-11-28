package seven;

import util.StringUtil;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Seven {
    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        second("input.txt");
    }

    private static void first(String s) throws URISyntaxException, IOException {
        final List<Integer> input = parseInput(s);
        var max = input.stream().mapToInt(i -> i)
                .max().orElse(0);
        var min = input.stream().mapToInt(i -> i)
                .min().orElse(0);

        var cost = IntStream.rangeClosed(min, max)
                .map(i -> calculateCostLinear(i, input))
                .min().orElse(0);
        System.out.println(cost);
    }

    private static void second(String s) throws URISyntaxException, IOException {
        final List<Integer> input = parseInput(s);
        var max = input.stream().mapToInt(i -> i)
                .max().orElse(0);
        var min = input.stream().mapToInt(i -> i)
                .min().orElse(0);

        var cost = IntStream.rangeClosed(min, max)
                .map(i -> calculateCostNonLinear(i, input))
                .min().orElse(0);
        System.out.println(cost);
    }

    private static int calculateCostNonLinear(final int to, final List<Integer> input) {
        return input.stream()
                .mapToInt(x -> Math.abs(x - to))
                .map(i -> {
                    if (i % 2 == 0) {
                        return (1 + i) * (i / 2);
                    } else {
                        return (1 + i) * (i / 2) + (i / 2 + 1);
                    }
                })
                .sum();
    }

    private static int calculateCostLinear(final int to, final List<Integer> input) {
        return input.stream()
                .mapToInt(x -> Math.abs(x - to))
                .sum();
    }

    private static List<Integer> parseInput(final String s) throws IOException, URISyntaxException {
        URL resource = Seven.class.getResource(s);
        assert resource != null;
        return Files.lines(Paths.get(resource.toURI()))
                .flatMap(StringUtil.split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
    }
}
