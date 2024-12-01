package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.List;
import java.util.stream.Stream;

import org.spick.utils.Zip;

public class One extends AbstractPuzzle<Integer> {

    public One(String in) {
        super(in);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        One one = new One("one.txt");
        System.out.println(one.first());
        System.out.println(one.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        Stream.Builder<Integer> left = Stream.builder();
        Stream.Builder<Integer> right = Stream.builder();
        getInput().forEach(l -> {
            left.add(l.left());
            right.add(l.right());
        });
        return new Zip<>(left.build().sorted(), right.build().sorted()).map((l, r) -> {
            return Math.abs(l - r);
        }).mapToInt(v -> v)
                .sum();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        Stream.Builder<Integer> left = Stream.builder();
        var right = new HashMap<Integer, Integer>();
        getInput().forEach(l -> {
            left.add(l.left());
            int rValue = l.right();
            right.computeIfPresent(rValue, (_, v) -> v + 1);
            right.computeIfAbsent(rValue, _ -> 1);
        });
        return left.build()
                .map(v -> v * right.getOrDefault(v, 0))
                .mapToInt(v -> v).sum();
    }

    private List<Line> getInput() {
        return streamLines().map(l -> {
            var cols = l.split("\s+");
            return new Line(Integer.parseInt(cols[0]), Integer.parseInt(cols[1]));
        }).toList();
    }

    private record Line(Integer left, Integer right) {
    };
}
