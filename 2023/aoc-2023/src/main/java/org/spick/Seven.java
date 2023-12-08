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

    // 250370104
    @Override
    public Long first() {
        var orderedHands = streamLines()
                .map(this::parseHandFirst)
                .sorted(handComparator(cardOrderFirst))
                .toList();
        var counter = new AtomicInteger(orderedHands.size());
        return orderedHands.stream()
                .mapToLong(Hand::value)
                .map(value -> value * counter.getAndDecrement())
                .sum();
    }

    // 251735672
    @Override
    public Long second() {
        var orderedHands = streamLines()
                .map(this::parseHandSecond)
                .sorted(handComparator(cardOrderSecond))
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
        return mapToParsedCard(cards, value);
    }

    private Hand parseHandSecond(String line) {
        var space = line.split(" ");
        var value = Integer.parseInt(space[1]);
        var cards = Arrays.asList(space[0].split(""));
        var firstMapping = mapToParsedCard(cards, value);
        var jCount = getjCount(firstMapping);
        return promote(firstMapping, jCount);
    }

    private static long getjCount(Hand hand) {
        var jCount = hand.raw().stream()
                .filter(s -> s.equals("J"))
                .count();
        return jCount;
    }

    private Hand promote(Hand hand, long jCount) {
        if (jCount == 0) {
            return hand;
        }
        return switch (hand) {
            case FiveOfAKind s -> s;
            case FourOfAKind s -> new FiveOfAKind(s.raw, s.value);
            case ThreeOfAKind s -> {
                if (getjCount(hand) == 3) {
                    // Special case if the triple is the jack, all we can do is go to Four
                    yield new FourOfAKind(s.raw, s.value);
                } else {
                    yield promote(new FourOfAKind(s.raw, s.value), --jCount);
                }
            }
            case FullHouse s -> new FiveOfAKind(s.raw, s.value);
            case TwoPair s -> {
                if (getjCount(hand) == 2) {
                    yield new FourOfAKind(s.raw, s.value);
                } else {
                    yield new FullHouse(s.raw(), s.value());
                }
            }
            case OnePair s -> {
                if (getjCount(hand) == 2) {
                    yield new ThreeOfAKind(s.raw, s.value);
                } else {
                    yield promote(new ThreeOfAKind(s.raw, s.value), --jCount);
                }
            }
            case HighCard s -> promote(new OnePair(s.raw, s.value), --jCount);
            default -> panic("panic3!");
        };
    }


    private Comparator<Map.Entry<String, List<String>>> groupLengthComparator() {
        return Comparator.comparingInt((Map.Entry<String, List<String>> e) -> e.getValue().size());
    }

    public Comparator<String> cardOrderComparator() {
        return Comparator.comparing(s -> cardOrderFirst.indexOf(s));
    }

    public Comparator<Hand> handComparator(final List<String> cardOrder) {
        return Comparator.comparing((Hand h) -> handOrder.get(h.getClass()))
                .thenComparing(secondaryComparator(0, cardOrder))
                .thenComparing(secondaryComparator(1, cardOrder))
                .thenComparing(secondaryComparator(2, cardOrder))
                .thenComparing(secondaryComparator(3, cardOrder))
                .thenComparing(secondaryComparator(4, cardOrder));
    }

    private Function<Hand, Integer> secondaryComparator(final int index, final List<String> cardOrder) {
        return h -> cardOrder.indexOf(h.raw().get(index));
    }

    private Hand mapToParsedCard(List<String> cards, int value) {
        var differentCards = cards.stream()
                .distinct()
                .count();
        var groups = cards.stream().collect(Collectors.groupingBy(Function.identity()));
        var labelEntry = groups.entrySet().stream()
                .max(groupLengthComparator()).get();
        var labelGroupSize = labelEntry.getValue().size();
        return switch ((int) differentCards) {
            case 1 -> new FiveOfAKind(cards, value);
            case 2 -> {
                if (labelGroupSize == 4) {
                    yield new FourOfAKind(cards, value);
                } else if (labelGroupSize == 3) {
                    yield new FullHouse(cards, value);
                } else {
                    yield panic("panic!");
                }
            }
            case 3 -> {
                if (labelGroupSize == 3) {
                    yield new ThreeOfAKind(cards, value);
                } else if (labelGroupSize == 2) {
                    yield new TwoPair(cards, value);
                } else {
                    yield panic("panic2!");
                }
            }
            case 4 -> new OnePair(cards, value);
            default -> new HighCard(cards, value);
        };
    }

    public interface Hand {
        List<String> raw();

        int value();
    }

    public record FiveOfAKind(List<String> raw, int value) implements Hand {
    }

    public record FourOfAKind(List<String> raw, int value) implements Hand {
    }

    public record FullHouse(List<String> raw, int value) implements Hand {
    }

    public record ThreeOfAKind(List<String> raw, int value) implements Hand {
    }

    public record TwoPair(List<String> raw, int value) implements Hand {
    }

    public record OnePair(List<String> raw, int value) implements Hand {
    }

    public record HighCard(List<String> raw, int value) implements Hand {
    }
}

