package org.spick;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Seven extends AbstractPuzzle<Long> {
    List<String> cardOrderFirst = List.of("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2");
    List<String> cardOrderSecond = List.of("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J");
    Map<Class<? extends Hand>, Integer> handOrder = Map.of(
            FiveOfAKind.class, 1,
            FourOfAKind.class, 2,
            FullHouse.class, 3,
            ThreeOfAKind.class, 4,
            TwoPair.class, 5,
            OnePair.class, 6,
            HighCard.class, 7
    );

    public Seven(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Seven("seven.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Long first() {
        var orderedHands = streamLines()
                .map(this::parseHandFirst)
                .sorted(handComparator())
                .toList();
        var counter = new AtomicInteger(orderedHands.size());
        return orderedHands.stream()
                .mapToLong(Hand::value)
                .map(value -> value * counter.getAndDecrement())
                .sum();
    }

    @Override
    public Long second() {
        var orderedHands = streamLines()
                .map(this::parseHandSecond)
                .sorted(handComparator())
                .toList();
        var counter = new AtomicInteger(orderedHands.size());
        return orderedHands.stream()
                .mapToLong(Hand::value)
                .map(value -> value * counter.getAndDecrement())
                .sum();
    }

    private Hand parseHandFirst(String line) {
        var space = line.split(" ");
        var value = Integer.parseInt(space[1]);
        var cards = Arrays.asList(space[0].split(""));
        var differentCards = cards.stream()
                .distinct()
                .count();
        var groups = cards.stream().collect(Collectors.groupingBy(Function.identity()));
        var labelEntry = groups.entrySet().stream()
                .max(groupLengthComparator().reversed()).get();
        var label = labelEntry.getKey();
        var labelGroupSize = labelEntry.getValue().size();
        return switch ((int) differentCards) {
            case 1 -> new FiveOfAKind(cards, label, value);
            case 2 -> {
                if (labelGroupSize == 4) {
                    yield new FourOfAKind(cards, label, value);
                } else if (labelGroupSize == 3) {
                    yield new FullHouse(cards, label, value);
                } else {
                    throw new RuntimeException("panic!");
                }
            }
            case 3 -> {
                if (labelGroupSize == 3) {
                    yield new ThreeOfAKind(cards, label, value);
                } else if (labelGroupSize == 2) {
                    yield new TwoPair(cards, label, value);
                } else {
                    throw new RuntimeException("panic2!");
                }
            }
            case 4 -> new OnePair(cards, label, value);
            default -> new HighCard(cards, label, value);
        };
    }

    private Hand parseHandSecond(String line) {
        var space = line.split(" ");
        var value = Integer.parseInt(space[1]);
        var cards = Arrays.asList(space[0].split(""));
        var differentCards = cards.stream()
                .distinct()
                .count();
        var groups = cards.stream().collect(Collectors.groupingBy(Function.identity()));
        var labelEntry = groups.entrySet().stream()
                .max(groupLengthComparator().reversed()).get();
        var label = labelEntry.getKey();
        var labelGroupSize = labelEntry.getValue().size();
        return switch ((int) differentCards) {
            case 1 -> new FiveOfAKind(cards, label, value);
            case 2 -> {
                if (labelGroupSize == 4) {
                    yield new FourOfAKind(cards, label, value);
                } else if (labelGroupSize == 3) {
                    yield new FullHouse(cards, label, value);
                } else {
                    throw new RuntimeException("panic!");
                }
            }
            case 3 -> {
                if (labelGroupSize == 3) {
                    yield new ThreeOfAKind(cards, label, value);
                } else if (labelGroupSize == 2) {
                    yield new TwoPair(cards, label, value);
                } else {
                    throw new RuntimeException("panic2!");
                }
            }
            case 4 -> new OnePair(cards, label, value);
            default -> new HighCard(cards, label, value);
        };
    }

    private Comparator<Map.Entry<String, List<String>>> groupLengthComparator() {
        return Comparator.comparingInt((Map.Entry<String, List<String>> e) -> e.getValue().size()).reversed()
                .thenComparingInt((Map.Entry<String, List<String>> e) -> e.getValue().stream().map(cardOrderFirst::indexOf).findFirst().get());
    }

    public Comparator<String> cardOrderComparator() {
        return Comparator.comparing(s -> cardOrderFirst.indexOf(s));
    }

    public Comparator<Hand> handComparator() {
        return Comparator.comparing((Hand h) -> handOrder.get(h.getClass()))
                .thenComparing(secondaryComparator(0))
                .thenComparing(secondaryComparator(1))
                .thenComparing(secondaryComparator(2))
                .thenComparing(secondaryComparator(3))
                .thenComparing(secondaryComparator(4));
    }

    private Function<Hand, Integer> secondaryComparator(final int index) {
        return h -> cardOrderFirst.indexOf(h.raw().get(index));
    }

    public interface Hand {
        List<String> raw();

        int value();

        String label();
    }

    public record FiveOfAKind(List<String> raw, String label, int value) implements Hand {
    }

    public record FourOfAKind(List<String> raw, String label, int value) implements Hand {
    }

    public record FullHouse(List<String> raw, String label, int value) implements Hand {
    }

    public record ThreeOfAKind(List<String> raw, String label, int value) implements Hand {
    }

    public record TwoPair(List<String> raw, String label, int value) implements Hand {
    }

    public record OnePair(List<String> raw, String label, int value) implements Hand {
    }

    public record HighCard(List<String> raw, String label, int value) implements Hand {
    }
}

