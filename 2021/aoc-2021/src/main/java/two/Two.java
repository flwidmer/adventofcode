package two;

import util.Pair;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.function.BinaryOperator;
import java.util.stream.Collectors;

public class Two {

    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        second("input.txt");
    }

    private static void first(String file) throws URISyntaxException, IOException {
        List<Pair<String, Integer>> list = getInput(file);

        Integer horizontal = getForward(list, "forward", 0, (x, y) -> x + y);
        Integer vertical = getForward(list, "down", 0, (x, y) -> x + y);
        vertical = getForward(list, "up", vertical, (x, y) -> x - y);
        System.out.println(horizontal * vertical);
    }

    private static List<Pair<String, Integer>> getInput(String file) throws IOException, URISyntaxException {
        URL resource = Two.class.getResource(file);
        List<Pair<String, Integer>> list = Files.lines(Paths.get(resource.toURI()))
                .map(s -> s.split("\\s"))
                .map(p -> new Pair<>(p[0], Integer.parseInt(p[1])))
                .collect(Collectors.toList());
        return list;
    }

    private static void second(String file) throws URISyntaxException, IOException {
        List<Pair<String, Integer>> list = getInput(file);
        var depth = 0;
        var horizontal = 0;
        var aim = 0;
        for (Pair<String, Integer> p: list) {
            switch (p.getL()) {
                case "forward":
                    horizontal += p.getR();
                    depth += aim * p.getR();
                    break;
                case "up":
                    aim -= p.getR();
                    break;
                case "down":
                    aim += p.getR();
                    break;
            }
        }
        System.out.println(horizontal * depth);
    }

    private static Integer getForward(List<Pair<String, Integer>> list, String keyword, int start, BinaryOperator<Integer> operation) {
        return list.stream()
                .filter(p -> p.getL().equals(keyword))
                .map(Pair::getR)
                .reduce(start, operation);
    }
}
