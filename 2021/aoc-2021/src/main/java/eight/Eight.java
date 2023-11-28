package eight;

import util.Pair;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Eight {
    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        second("input.txt");
    }

    private static void first(String fileName) throws URISyntaxException, IOException {
        final long count = parseInput(fileName)
                .map(Pair::getR)
                .flatMap(List::stream)
                .map(String::length)
                .filter(uniqueLengthFiler())
                .count();

        System.out.println(count);
    }

    private static void second(String fileName) throws URISyntaxException, IOException {
        final long sum = parseInput(fileName)
                .map(calculateValue())
                .mapToInt(x -> x)
                .sum();
        System.out.println(sum);
    }

    private static Function<Pair<List<String>, List<String>>, Integer> calculateValue() {
        return p -> {
            var all = p.getL();
            Map<Integer, String> mappings = new HashMap<>();
            //find 1
            all.stream()
                    .filter(f -> f.length() == 2) // length 2
                    .forEach(f -> mappings.put(1, f));
            //find 4
            all.stream()
                    .filter(f -> f.length() == 4) // length 4
                    .forEach(f -> mappings.put(4, f));
            //find 7
            all.stream()
                    .filter(f -> f.length() == 3) // length 3
                    .forEach(f -> mappings.put(7, f));
            // find 8
            all.stream()
                    .filter(f -> f.length() == 7) // length 7
                    .forEach(f -> mappings.put(8, f));

            // find 9
            all.stream()
                    .filter(f -> f.length() == 6) // length 6
                    .filter(f -> containsAll(f, mappings.get(4))) // contains all of 4
                    .forEach(f -> mappings.put(9, f));

            // find 0
            all.stream()
                    .filter(f -> f.length() == 6) // length 6
                    .filter(f -> !f.equals(mappings.get(9))) // is not 9
                    .filter(f -> containsAll(f, mappings.get(1))) // contains all of 1
                    .forEach(f -> mappings.put(0, f));

            // find 6
            all.stream()
                    .filter(f -> f.length() == 6) // length 6
                    .filter(f -> !f.equals(mappings.get(0))) // is not 9
                    .filter(f -> !f.equals(mappings.get(9))) // is not 0
                    .forEach(f -> mappings.put(6, f));

            // find 3
            all.stream()
                    .filter(f -> f.length() == 5) // length 5
                    .filter(f -> containsAll(mappings.get(9), f)) // 9 contains all of 3
                    .filter(f -> containsAll(f, mappings.get(1))) // 1 contains all of 3
                    .forEach(f -> mappings.put(3, f));

            // find 5
            all.stream()
                    .filter(f -> f.length() == 5) // length 5
                    .filter(f -> containsAll(mappings.get(9), f)) // 9 contains all of 5
                    .filter(f -> !f.equals(mappings.get(3))) // is not 3
                    .forEach(f -> mappings.put(5, f));

            // find 2
            all.stream()
                    .filter(f -> f.length() == 5) // length 5
                    .filter(f -> !f.equals(mappings.get(5))) // is not 5
                    .filter(f -> !f.equals(mappings.get(3))) // is not 3
                    .forEach(f -> mappings.put(2, f));

            //calculate value
            var invertedMappings = mappings.entrySet().stream()
                    .collect(Collectors.toMap(Map.Entry::getValue, Map.Entry::getKey));
            var stringValue = p.getR().stream()
                    .map(invertedMappings::get)
                    .map(i -> Integer.toString(i))
                    .collect(Collectors.joining());
            return Integer.parseInt(stringValue);
        };

    }

    private static boolean containsAll(final String f, final String s) {
        return Arrays.stream(s.split(""))
                .allMatch(f::contains);
    }

    private static Predicate<? super Integer> uniqueLengthFiler() {
        return l -> l == 2 || l == 3 || l == 4 || l == 7;
    }

    private static Stream<Pair<List<String>, List<String>>> parseInput(final String fileName) throws IOException, URISyntaxException {
        URL resource = Eight.class.getResource(fileName);
        assert resource != null;
        return Files.lines(Paths.get(resource.toURI()))
                .map(s -> s.split(" \\| "))
                .map(a -> new Pair<>(a[0], a[1]))
                .map(splitDigits());
    }

    private static Function<Pair<String, String>, Pair<List<String>, List<String>>> splitDigits() {
        return p -> new Pair<>(
                Arrays.stream(p.getL().split(" "))
                        .map(sortCharacters())
                        .collect(Collectors.toList()),
                Arrays.stream(p.getR().split(" "))
                        .map(sortCharacters())
                        .collect(Collectors.toList()));
    }

    private static Function<String, String> sortCharacters() {
        return a -> {
            var c = a.toCharArray();
            Arrays.sort(c);
            return new String(c);
        };
    }
}
