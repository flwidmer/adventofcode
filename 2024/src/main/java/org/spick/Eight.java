package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class Eight extends AbstractPuzzle<Integer> {
    public Eight(String input) {
        super(input);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        var puzzle = new Eight("eight.txt");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        var cityMap = getInput();
        var result = new HashSet<Position>();
        for (var antennas : cityMap.getFrequencyIndex().values()) {
            for (var i = 0; i < antennas.size(); i++) {
                for (var k = i + 1; k < antennas.size(); k++) {
                    var a = antennas.get(i).location();
                    var b = antennas.get(k).location();
                    var vector = a.minus(b);
                    var one = a.plus(vector);
                    if (cityMap.isInBounds(one)) {
                        result.add(one);
                    }
                    var two = b.minus(vector);
                    if (cityMap.isInBounds(two)) {
                        result.add(two);
                    }
                }
            }
        }
        return result.size();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var cityMap = getInput();
        var result = new HashSet<Position>();
        for (var antennas : cityMap.getFrequencyIndex().values()) {
            for (var i = 0; i < antennas.size(); i++) {
                for (var k = i + 1; k < antennas.size(); k++) {
                    var a = antennas.get(i).location();
                    var b = antennas.get(k).location();
                    result.add(a);
                    result.add(b);
                    var vector = a.minus(b);
                    var one = a.plus(vector);
                    while (cityMap.isInBounds(one)) {
                        result.add(one);
                        one = one.plus(vector);
                    }
                    var two = b.minus(vector);
                    while (cityMap.isInBounds(two)) {
                        result.add(two);
                        two = two.minus(vector);
                    }
                }
            }
        }
        return result.size();
    }

    public CityMap getInput() {
        var input = streamLines().toList();
        var antennas = new ArrayList<Antenna>();
        int maxY = input.size();
        int maxX = input.get(0).length();
        for (int y = 0; y < maxY; y++) {
            var yLine = input.get(y);
            for (int x = 0; x < maxX; x++) {
                if (yLine.charAt(x) != '.') {
                    antennas.add(new Antenna(new Position(x, y), yLine.charAt(x)));
                }
            }
        }
        return new CityMap(antennas, maxX, maxY);
    }

    public record CityMap(List<Antenna> antennas, long maxX, long maxY) {

        public boolean isInBounds(Position position) {
            return position.x() >= 0 && position.x() < maxX() && position.y() >= 0 && position.y() < maxY;
        }

        public Map<Character, List<Antenna>> getFrequencyIndex() {
            return antennas().stream()
                    .collect(Collectors.groupingBy(Antenna::frequency));
        }
    };

    public record Antenna(Position location, char frequency) {
    };

    public record Position(long x, long y) {
        public Position minus(Position other) {
            return new Position(x() - other.x(), y() - other.y());
        }

        public Position plus(Position other) {
            return new Position(x() + other.x(), y() + other.y());
        }
    };

}
