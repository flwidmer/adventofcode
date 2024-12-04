package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Three extends AbstractPuzzle<Integer> {

    private static Pattern REGEX = Pattern.compile("(do\\(\\))|(don't\\(\\))|(mul\\((\\d{1,3}),(\\d{1,3})\\))");

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
        return new Parser(false, readInput()).parse().stream()
                .mapToInt(a -> a)
                .sum();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        return new Parser(true, readInput()).parse().stream()
                .mapToInt(a -> a)
                .sum();
    }

    private class Parser {
        private boolean conditional;
        private boolean enabled = true;
        private CharSequence input;

        public Parser(boolean conditional, String input) {
            this.conditional = conditional;
            this.input = input;
        }

        private Matcher getMatcher() {
            var matcher = REGEX.matcher(input);
            return matcher;
        }

        public List<Integer> parse() {
            var matcher = getMatcher();
            enabled = true;
            var multiplications = new ArrayList<Integer>();
            while (matcher.find()) {
                multiplications.add(nextInt(matcher));
            }
            return multiplications;
        }

        private Integer nextInt(Matcher matcher) {
            if (conditional && matcher.group(1) != null) {
                enabled = true;
            } else if (conditional && matcher.group(2) != null) {
                enabled = false;
            } else if (enabled && matcher.group(3) != null) {
                return Integer.parseInt(matcher.group(4)) * Integer.parseInt(matcher.group(5));
            }
            return 0;
        }
    }
}
