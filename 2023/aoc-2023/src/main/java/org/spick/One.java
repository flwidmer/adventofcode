package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;

import static java.util.function.Function.identity;
import static java.util.stream.Collectors.reducing;

public class One extends AbstractPuzzle<Integer> {
    public One(String s) {
        super(s);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        var puzzle = new One("one.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        return Files.lines(getFilePath())
                .map(x -> x.replaceAll("[^0-9]", ""))
                .map(x -> x.charAt(0) + x.substring(x.length() - 1))
                .mapToInt(Integer::parseInt)
                .sum();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        return Files.lines(getFilePath())
                .map(this::replacerFunction)
                .map(x -> x.replaceAll("[^0-9]", ""))
                .map(x -> x.charAt(0) + x.substring(x.length() - 1))
                .mapToInt(Integer::parseInt)
                .sum();
    }


    public String replacerFunction(String in) {
        AtomicReference<String> currentString = new AtomicReference<>(in);

        var replacements = Map.of("one", "1",
                "two", "2",
                "three", "3",
                "four", "4",
                "five", "5",
                "six", "6",
                "seven", "7",
                "eight", "8",
                "nine", "9");
        Function<String, String> fun = replacements.entrySet().stream()
                .map(this::replace)
                .reduce(identity(), Function::compose);
        return fun.apply(in);
    }

    private Function<String, String> replace(Map.Entry<String, String> e) {
        return s -> s.replaceAll(e.getKey(), e.getKey() + e.getValue() + e.getKey());
    }
}
