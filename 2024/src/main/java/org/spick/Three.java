package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Three extends AbstractPuzzle<Integer> {

    public Three(String input) {
        super(input);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        Three three = new Three("three.txt");
        System.out.println(three.first());
        System.out.println(three.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        var input = streamLines().collect(Collectors.joining());
        var regex = Pattern.compile("mul\\((\\d{1,3}),(\\d{1,3})\\)");
        var matcher = regex.matcher(input);
        var multiplications = new ArrayList<Integer>();
        while (matcher.find()) {
            multiplications
                    .add(Integer.parseInt(matcher.group(1)) * Integer.parseInt(matcher.group(2)));
        }
        return multiplications.stream()
                .mapToInt(a -> a)
                .sum();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var input = streamLines().collect(Collectors.joining());
        var regex = Pattern.compile("(do\\(\\))|(don't\\(\\))|(mul\\((\\d{1,3}),(\\d{1,3})\\))");
        var matcher = regex.matcher(input);
        var multiplications = new ArrayList<Integer>();
        var enabled = true;
        while (matcher.find()) {
            if (matcher.group(1) != null) {
                enabled = true;
            } else if (matcher.group(2) != null) {
                enabled = false;
            } else if (enabled && matcher.group(3) != null) {
                multiplications.add(Integer.parseInt(matcher.group(4)) * Integer.parseInt(matcher.group(5)));
            }
        }
        return multiplications.stream()
                .mapToInt(a -> a)
                .sum();
    }
}
