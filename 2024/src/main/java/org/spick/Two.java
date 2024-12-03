package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.stream.Gatherers;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Two extends AbstractPuzzle<Long> {

    public Two(String input) {
        super(input);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        Two two = new Two("two.txt");
        System.out.println(two.first());
        System.out.println(two.second());
    }

    @Override
    public Long first() throws IOException, URISyntaxException {
        return getInput().map(report -> {
            return isSafe(report);
        })
                .filter(Predicate.isEqual(true))
                .count();
    }

    @Override
    public Long second() throws IOException, URISyntaxException {
        return getInput().map(report -> {
            if (isSafe(report)) {
                return true;
            }
            return IntStream.range(0, report.size()).mapToObj(index -> {
                var removed = new ArrayList<>(report);
                removed.remove(index);
                return isSafe(removed);
            }).anyMatch(p -> p);
        })
                .filter(Predicate.isEqual(true))
                .count();
    }

    private boolean isSafe(Collection<Integer> report) {
        List<Integer> diffs = report.stream().gather(Gatherers.windowSliding(2))
                .map(l -> l.getFirst() - l.getLast())
                .toList();
        return diffs.stream().allMatch(v -> v >= -3 && v <= -1) || diffs.stream().allMatch(v -> v >= 1 && v <= 3);
    }

    private Stream<List<Integer>> getInput() {
        return streamLines().map(line -> line.split("\s"))
                .map(Arrays::stream)
                .map(s -> s.map(Integer::parseInt))
                .map(Stream::toList)
                .map(ArrayList::new);
    }
}
