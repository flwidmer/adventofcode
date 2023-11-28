package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.Set;
import java.util.function.ToIntFunction;
import java.util.stream.Collectors;

public class Three extends AbstractPuzzle<Integer> {

    public static void main(String[] args) throws IOException, URISyntaxException {
        var puzzle = new Three("three.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    public Three(String s) {
        super(s);
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        return Files.lines(getFilePath())
                .mapToInt(calculateCommonItem())
                .sum();
    }

    private ToIntFunction<? super String> calculateCommonItem() {
        return s -> {
            var length = s.length();
            var compartment1 = s.substring(0, length / 2);
            var compartment2 = s.substring(length / 2);
            var c1set = createSet(compartment1);
            var common = compartment2.chars()
                    .filter(c -> c1set.contains(c))
                    .findAny().orElse(-1);
            return mapLetter((char) common);
        };
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var lines = Files.readAllLines(getFilePath()).iterator();
        var sum = 0;
        while (lines.hasNext()) {
            sum += findBadge(lines.next(), lines.next(), lines.next());
        }
        return sum;
    }

    private int findBadge(String one, String two, String three) {
        var set1 = createSet(one);
        var set2 = createSet(two);
        var badge = three.chars()
                .filter(set1::contains)
                .filter(set2::contains)
                .findAny().orElse(-1);
        return mapLetter((char) badge);
    }

    public int mapLetter(char letter) {
        if (letter > 96) {
            return letter - 96;
        }
        if (letter <= 96) {
            return letter - 38;
        }
        return -1;
    }

    private static Set<Integer> createSet(String compartment1) {
        return compartment1.chars()
                .mapToObj(x -> x)
                .collect(Collectors.toSet());
    }
}
