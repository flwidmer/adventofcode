package six;

import util.StringUtil;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Six {

    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        second("input.txt");
    }

    private static void first(String s) throws URISyntaxException, IOException {
        calculate(80, s);
    }

    private static void second(String s) throws URISyntaxException, IOException {
        calculate(256, s);
    }

    private static void calculate(final int generations, final String s) throws IOException, URISyntaxException {
        URL resource = Six.class.getResource(s);
        Map<Integer, Long> input = Files.lines(Paths.get(resource.toURI()))
                .flatMap(StringUtil.split(","))
                .map(Integer::parseInt)
                .collect(Collectors.toMap(Function.identity(), x -> 1L, Long::sum));

        for (int i = 0; i < generations; i++) {
            input = calculateNextGeneration(input);
        }
        var count = input.values().stream()
                .mapToLong(i-> i)
                .sum();
        System.out.println(count);
    }

    private static Map<Integer, Long> calculateNextGeneration(final Map<Integer, Long> input) {
        Map<Integer, Long> nextGeneration = new HashMap<>();
        for (final Map.Entry<Integer, Long> entry : input.entrySet()) {
            if (entry.getKey().equals(0)) {
                nextGeneration.put(6, nextGeneration.getOrDefault(6, 0L) + entry.getValue());
                nextGeneration.put(8, nextGeneration.getOrDefault(8, 0L) + entry.getValue());
            } else {
                final Integer newKey = entry.getKey() - 1;
                nextGeneration.put(newKey, nextGeneration.getOrDefault(newKey, 0L) + entry.getValue());
            }
        }
        return nextGeneration;
    }
}
