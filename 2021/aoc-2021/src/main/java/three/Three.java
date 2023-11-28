package three;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.BiPredicate;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

public class Three {

    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");
        second("test.txt");
        second("input.txt");
    }

    private static void first(String s) throws URISyntaxException, IOException {
        URL resource = Three.class.getResource(s);
        var collect = Files.lines(Paths.get(resource.toURI()))
                .map(String::toCharArray)
                .collect(Collectors.toList());

        var gamma = new Stack<Integer>();
        var epsilon = new Stack<Integer>();
        var length = collect.get(0).length;
        for (int i = 0; i < length; i++) {
            treatColumn(collect, gamma, epsilon, i);
        }
        int g = toBinary(gamma);
        int e = toBinary(epsilon);
        System.out.println(g * e);
    }

    private static int toBinary(Stack<Integer> binary) {
        var result = 0;
        int position = 0;
        while (!binary.isEmpty()) {
            var current = binary.pop();
            current = current << position;
            result += current;
            position++;
        }
        return result;
    }

    private static void treatColumn(List<char[]> collect, Stack<Integer> gamma, Stack<Integer> epsilon, int i) {
        Map<Character, List<Character>> column = getColumn(collect, i);
        if (column.get('0').size() > column.get('1').size()) {
            gamma.push(0);
            epsilon.push(1);
        } else {
            gamma.add(1);
            epsilon.push(0);
        }
    }

    private static Map<Character, List<Character>> getColumn(List<char[]> collect, int i) {
        return collect.stream().map(a -> a[i])
                .collect(Collectors.groupingBy(Function.identity()));
    }

    private static void second(String s) throws URISyntaxException, IOException {
        URL resource = Three.class.getResource(s);
        var input = Files.lines(Paths.get(resource.toURI()))
                .map(String::toCharArray)
                .collect(Collectors.toList());
        Stack<Integer> oxygenCharacters = calcOxgenOrCarbon(input,  (x, y) -> x > y);
        Stack<Integer> carbonCharacters = calcOxgenOrCarbon(input,  (x, y) -> x <= y);
        System.out.println(toBinary(oxygenCharacters) * toBinary(carbonCharacters));
    }

    private static Stack<Integer> calcOxgenOrCarbon(List<char[]> input, BiPredicate<Integer, Integer> comparison) {
        List<char[]> collect = oxygenCarbon(input, 0, comparison);
        char[] result = collect.get(0);
        Stack<Integer> oxygenCharacters = new Stack<>();
        for (char c : result) {
            var b = (c == '0') ? 0 : 1;
            oxygenCharacters.push(b);
        }
        return oxygenCharacters;
    }

    private static List<char[]> oxygenCarbon(List<char[]> collect, int pos, BiPredicate<Integer, Integer> comparison) {
        if (collect.size() == 1) {
            return collect;
        }
        var col = getColumn(collect, pos);
        var filter = 'x';
        if (comparison.test(getSize(col, '0') , getSize(col, '1'))) {
            filter = '0';
        } else {
            filter = '1';
        }
        var filtered = collect.stream().filter(getFilter(pos, filter)).collect(Collectors.toList());
        return oxygenCarbon(filtered, ++pos, comparison);
    }



    private static int getSize(Map<Character, List<Character>> col, char key) {
        return col.getOrDefault(key, Collections.emptyList()).size();
    }

    private static Predicate<? super char[]> getFilter(int i, char filter) {
        return t -> t[i] == filter;
    }


}
