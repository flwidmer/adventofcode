package one;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public class One {

    public static void main(String[] args) throws IOException, URISyntaxException {
        first("one_test.txt");
        first("one.txt");
        second("one_test.txt");
        second("one.txt");
    }

    private static void second(String name) throws URISyntaxException, IOException {
        URL resource = One.class.getResource(name);
        List<Integer> input = Files.lines(Paths.get(resource.toURI()))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
        Integer count = 0;
        var last = 0;
        for(int i = 0; i < input.size() -3; i++) {
            var sum = input.get(i) + input.get(i+1) + input.get(i+2);
            if (sum > last) {
                count++;
            }
            last = sum;
        }
        System.out.println(count);
    }

    private static void first(String name) throws IOException, URISyntaxException {
        URL resource = One.class.getResource(name);
        List<Integer> input = Files.lines(Paths.get(resource.toURI()))
                .map(Integer::parseInt)
                .collect(Collectors.toList());
        Integer last = input.get(0);
        Integer count = 0;
        for(Integer i:input) {
            if (i > last) {
                count++;
            }
            last = i;
        }
        System.out.println(count);
    }
}
