package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Three extends AbstractPuzzle<Integer> {

    private boolean enabled = true;
    private Pattern REGEX = Pattern.compile("(do\\(\\))|(don't\\(\\))|(mul\\((\\d{1,3}),(\\d{1,3})\\))");

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
        var matcher = getMatcher();
        var multiplications = new ArrayList<Integer>();
        while (matcher.find()) {
            multiplications.add(nextInt(matcher, false));
        }
        return multiplications.stream()
                .mapToInt(a -> a)
                .sum();
    }

    private Matcher getMatcher() {
        var input = streamLines().collect(Collectors.joining());
        
        var matcher = REGEX.matcher(input);
        return matcher;
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var matcher = getMatcher();
        var multiplications = new ArrayList<Integer>();
        enabled = true;
        
        while (matcher.find()) {
            multiplications.add(nextInt(matcher, true));
        }
        return multiplications.stream()
                .mapToInt(a -> a)
                .sum();
    }

    private Integer nextInt(Matcher matcher, boolean conditional){
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
