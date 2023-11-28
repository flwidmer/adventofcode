package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.function.Function;

public class One extends AbstractPuzzle<Integer> {

    public One(String s) {
        super(s);
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        final var values = calculateElves();
        return values.stream().sorted(Comparator.reverseOrder()).findFirst().get();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        final var values = calculateElves();
        return values.stream().sorted(Comparator.reverseOrder()).limit(3).mapToInt(x -> x).sum();
    }

    private ArrayList<Integer> calculateElves() throws IOException, URISyntaxException {
        final var lines = Files.readAllLines(getFilePath());
        final ArrayList<Integer> values = new ArrayList<>();
        final int[] current = {0};

        for (String line : lines) {
            switch (parse().apply(line)) {
                case Break _:
                    values.add(current[0]);
                    current[0] = 0;
                    break;
                case Calories calories:
                    current[0] += calories.calories();
                    break;
                default:
            }
        }
        values.add(current[0]);
        return values;
    }

    private Function<? super String, ?> parse() {
        return s -> {
            try {
                return new Calories(Integer.valueOf(s));
            } catch (NumberFormatException e) {
                return new Break();
            }
        };
    }

    record Calories(int calories) {
    }

    record Break() {
    }
}
