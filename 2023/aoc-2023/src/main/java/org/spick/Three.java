package org.spick;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Three extends AbstractPuzzle<Integer> {

    private boolean debug = false;

    public Three(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Three("three.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() {
        var lines = streamLines().toList();

        // for each line, find number ranges
        var parsedLines = IntStream.range(0, lines.size())
                .mapToObj(i -> new Line(i, lines.get(i))) // candidate for util
                .map(this::parseLine)
                .toList();
        var sum = 0;
        for (ParsedLine p : parsedLines) {
            for (NumberRange r : p.ranges()) {
                // check line if any adjacent is
                if (checkInline(p, r)) {
                    sum += getNumber(p, r);
                    continue;
                }
                // check line up
                if (checkAdjacentLine(lines, p.number() - 1, r)) {
                    sum += getNumber(p, r);
                    continue;
                }
                // check line down
                if (checkAdjacentLine(lines, p.number() + 1, r)) {
                    sum += getNumber(p, r);
                    continue;
                }
            }
        }
        return sum;
    }

    private boolean checkAdjacentLine(List<String> lines, int number, NumberRange r) {
        if (number < 0 || number >= lines.size()) {
            return false;
        }
        var line = lines.get(number);
        var region = line.substring(normalizeBeginIndex(r.beginInclusive() - 1), normalizeEndIndex(r.endInclusive() + 2, line.length()));
        return region.chars().anyMatch(this::isSymbol);
    }

    private boolean checkInline(ParsedLine p, NumberRange r) {
        var before = checkIsSymbol(p, r.beginInclusive - 1);
        var after = checkIsSymbol(p, r.endInclusive + 1);
        return before || after;
    }

    private boolean checkIsSymbol(ParsedLine p, int index) {
        if (index < 0 || index >= p.line().length()) {
            return false;
        }
        return isSymbol(p.line.charAt(index));
    }

    private ParsedLine parseLine(Line l) {
        return new ParsedLine(l.number(), l.line(), lineToRegions(l.line()));
    }

    private List<NumberRange> lineToRegions(String line) {
        var regions = new ArrayList<NumberRange>();
        var matching = false;
        var currentBegin = 0;
        for (var i = 0; i < line.length(); i++) {
            var currentChar = line.charAt(i);
            if (isDigit(currentChar) && !matching) {
                matching = true;
                currentBegin = i;
            } else if (!isDigit(currentChar) && matching) {
                matching = false;
                regions.add(new NumberRange(currentBegin, i - 1));
            }
        }
        // line ended
        if (matching) {
            regions.add(new NumberRange(currentBegin, line.length() - 1));
        }
        return regions;
    }

    private boolean isDigit(char c) {
        return 48 <= c && c <= 57;
    }

    private boolean isSymbol(char c) {
        return !isDigit(c) && c != '.';
    }

    private boolean isSymbol(int c) {
        return !isDigit((char) c) && c != '.';
    }

    private int normalizeEndIndex(int index, int size) {
        return Math.min(index, size);
    }

    private static int normalizeBeginIndex(int index) {
        return Math.max(0, index);
    }

    private int getNumber(ParsedLine p, NumberRange r) {
        return Integer.parseInt(p.line().substring(r.beginInclusive(), r.endInclusive() + 1));
    }

    @Override
    public Integer second() {
        var lines = streamLines().toList();

        // for each line, find number ranges
        var parsedLines = IntStream.range(0, lines.size())
                .mapToObj(i -> new Line(i, lines.get(i))) // candidate for util
                .map(this::parseLine)
                .toList();
        var sum = 0;
        for (int i = 0; i < parsedLines.size(); i++) {
            var previousLine = Optional.of(i - 1).filter(n -> n >= 0).map(parsedLines::get);
            var currentLine = Optional.of(i).map(parsedLines::get);
            var nextLine = Optional.of(i + 1).filter(n -> n < parsedLines.size()).map(parsedLines::get);
            // for each gear
            var stringLine = currentLine.get().line();
            var gears = IntStream.range(0, stringLine.length())
                    .filter(index -> stringLine.charAt(index) == '*')
                    .boxed()
                    .toList();
            for (Integer gear : gears) {
                // check in line
                var gearValues = Stream.of(currentLine, nextLine, previousLine)
                        .flatMap(Optional::stream)
                        .flatMap(p -> getNumberAdjacentTo(gear, p))
                        .toList();
                if (gearValues.size() == 2) {
                    sum += gearValues.get(0) * gearValues.get(1);
                } else if (gearValues.size() > 2) {
                    System.out.println("panic");
                } else {
                    System.out.println("no gear found");
                }
            }
        }
        return sum;
    }

    private Stream<Integer> getNumberAdjacentTo(Integer gear, ParsedLine p) {
        return p.ranges().stream()
                .filter(r -> r.touches(gear))
                .map(r -> getNumber(p, r));
    }

    public record Line(int number, String line) {
    }

    public record ParsedLine(int number, String line, List<NumberRange> ranges) {

    }

    public record NumberRange(int beginInclusive, int endInclusive) {
        public boolean touches(int position) {
            return beginInclusive - 1 <= position && position <= endInclusive + 1;
        }
    }
}
