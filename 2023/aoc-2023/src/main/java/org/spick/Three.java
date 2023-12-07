package org.spick;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Stream;

public class Three extends AbstractPuzzle<Integer> {

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
        var parsedLines = streamLines()
                .map(this::parseLine)
                .toList();
        var sum = 0;
        for (int i = 0; i < parsedLines.size(); i++) {
            var previousLine = Optional.of(i - 1).filter(n -> n >= 0).map(parsedLines::get);
            var currentLine = Optional.of(i).map(parsedLines::get);
            var nextLine = Optional.of(i + 1).filter(n -> n < parsedLines.size()).map(parsedLines::get);
            for (NumberRange r : currentLine.get().ranges()) {
                var touchesSymbol = Stream.of(currentLine, nextLine, previousLine)
                        .flatMap(Optional::stream)
                        .map(ParsedLine::symbolPositions)
                        .flatMap(List::stream)
                        .anyMatch(r::touches);
                // check line if any adjacent is
                if (touchesSymbol) {
                    sum += getNumber(currentLine.get(), r);
                }
            }
        }
        return sum;
    }

    @Override
    public Integer second() {
        var parsedLines = streamLines()
                .map(this::parseLine)
                .toList();
        var sum = 0;
        for (int i = 0; i < parsedLines.size(); i++) {
            var previousLine = Optional.of(i - 1).filter(n -> n >= 0).map(parsedLines::get);
            var currentLine = Optional.of(i).map(parsedLines::get);
            var nextLine = Optional.of(i + 1).filter(n -> n < parsedLines.size()).map(parsedLines::get);
            // for each gear
            sum += parsedLines.get(i).symbolPositions().stream()
                    .filter(s -> s.c() == '*')
                    .mapToInt(gear -> {
                        // check in line
                        var gearValues = Stream.of(currentLine, nextLine, previousLine)
                                .flatMap(Optional::stream)
                                .flatMap(p -> getNumberAdjacentTo(gear, p))
                                .toList();
                        if (gearValues.size() == 2) {
                            return gearValues.get(0) * gearValues.get(1);
                        } else if (gearValues.size() > 2) {
                            System.out.println("panic");
                        }
                        return 0;
                    }).sum();
        }
        return sum;
    }

    private ParsedLine parseLine(String line) {
        var regions = new ArrayList<NumberRange>();
        var symbols = new ArrayList<SymbolPosition>();
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
            if (isSymbol(currentChar)) {
                symbols.add(new SymbolPosition(i, currentChar));
            }
        }
        // line ended
        if (matching) {
            regions.add(new NumberRange(currentBegin, line.length() - 1));
        }
        return new ParsedLine(line, regions, symbols);
    }

    private boolean isDigit(char c) {
        return 48 <= c && c <= 57;
    }

    private boolean isSymbol(char c) {
        return !isDigit(c) && c != '.';
    }

    private int getNumber(ParsedLine p, NumberRange r) {
        return Integer.parseInt(p.line().substring(r.beginInclusive(), r.endInclusive() + 1));
    }

    private Stream<Integer> getNumberAdjacentTo(SymbolPosition gear, ParsedLine p) {
        return p.ranges().stream()
                .filter(r -> r != null)
                .filter(r -> r.touches(gear))
                .map(r -> getNumber(p, r));
    }

    public record ParsedLine(String line, List<NumberRange> ranges, List<SymbolPosition> symbolPositions) {

    }

    public record NumberRange(int beginInclusive, int endInclusive) {
        public boolean touches(SymbolPosition s) {
            return beginInclusive - 1 <= s.position() && s.position() <= endInclusive + 1;
        }
    }

    public record SymbolPosition(int position, char c) {
        public boolean touches(NumberRange range) {
            return range.beginInclusive() - 1 <= position && position <= range.endInclusive() + 1;
        }
    }
}
