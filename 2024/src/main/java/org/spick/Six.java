package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

import org.spick.utils.Pair;

public class Six extends AbstractPuzzle<Integer> {

    public Six(String string) {
        super(string);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        Six six = new Six("six.txt");
        System.out.println(six.first());
        System.out.println(six.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        var map = getMap();
        return doRun(map).size();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var map = getMap();
        var counter = 0;
        var guardPath = doRun(map);
        for(var position : guardPath) {
            var newMap = makeObstacle(map, position.getLeft(), position.getRight());
            if (loopDetect(newMap)) {
                counter++;
            }
        }
        return counter;
    }

    private List<List<String>> makeObstacle(List<List<String>> map, int i, int k) {
        var newMap = new ArrayList<List<String>>();
        for (var line : map) {
            newMap.add(new ArrayList<>(line));
        }
        newMap.get(i).set(k, "#");
        return newMap;
    }

    private HashSet<Pair<Integer, Integer>> doRun(List<List<String>> map) {
        var currentPosition = startPosition(map);
        var visitedPositions = new HashSet<Pair<Integer, Integer>>();
        var currentDirection = new Pair<>(-1, 0);
        while (inBounds(currentPosition, map)) {
            visitedPositions.add(currentPosition);
            var nextPosition = getNextPosition(currentPosition, currentDirection);
            if (readPosition(map, nextPosition).equals("#")) {
                currentDirection = rotateRight(currentDirection);
                nextPosition = getNextPosition(currentPosition, currentDirection);
            }
            currentPosition = nextPosition;
        }
        return visitedPositions;
    }

    private boolean loopDetect(List<List<String>> map) {
        var visitedPositions = new HashSet<>();
        var currentPosition = startPosition(map);
        var currentDirection = new Pair<>(-1, 0);
        while (inBounds(currentPosition, map)) {
            var state = new Pair<>(currentPosition, currentDirection);
            if (visitedPositions.contains(state)) {
                return true;
            }
            visitedPositions.add(state);
            var nextPosition = getNextPosition(currentPosition, currentDirection);
            var rotations = 0;
            while (readPosition(map, nextPosition).equals("#")) {
                currentDirection = rotateRight(currentDirection);
                rotations++;
                if(rotations > 4) {
                    return true;
                }
                nextPosition = getNextPosition(currentPosition, currentDirection);
            }
            currentPosition = nextPosition;
        }
        return false;
    }

    private boolean inBounds(Pair<Integer, Integer> currentPosition, List<List<String>> map) {
        return currentPosition.getLeft() >= 0 && currentPosition.getLeft() < map.size()
                && currentPosition.getRight() >= 0 && currentPosition.getRight() < map.get(0).size();
    }

    private Pair<Integer, Integer> startPosition(List<List<String>> map) {
        for (int i = 0; i < map.size(); i++) {
            for (int k = 0; k < map.size(); k++) {
                if (map.get(i).get(k).equals("^")) {
                    return new Pair<>(i, k);
                }
            }
        }
        return new Pair<>(0, 0);
    }

    private String readPosition(List<List<String>> map, Pair<Integer, Integer> toRead) {
        if (!inBounds(toRead, map)) {
            return "X";
        }
        return map.get(toRead.getLeft()).get(toRead.getRight());
    }

    private Pair<Integer, Integer> getNextPosition(Pair<Integer, Integer> currentPosition, Pair<Integer, Integer> currentDirection) {
        return new Pair<>(currentPosition.getLeft() + currentDirection.getLeft(),
                currentPosition.getRight() + currentDirection.getRight());
    }

    private Pair<Integer, Integer> rotateRight(Pair<Integer, Integer> currentDirection) {
        return new Pair<Integer, Integer>(currentDirection.getRight(), currentDirection.getLeft() * -1);
    }

    private List<List<String>> getMap() {
        return streamLines()
                .map(line -> line.split(""))
                .map(splitLine -> Arrays.stream(splitLine).toList())
                .toList();
    }
}
