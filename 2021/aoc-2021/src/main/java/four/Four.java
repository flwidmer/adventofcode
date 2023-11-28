package four;

import util.Pair;

import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Four {

    public static void main(String[] args) throws URISyntaxException, IOException {
        first("test.txt");
        first("input.txt");

        second("test.txt");
        second("input.txt");
    }

    private static void first(String name) throws URISyntaxException, IOException {
        URL resource = Four.class.getResource(name);
        assert resource != null;
        List<String[]> input = readFile(resource);
        var firstLine = input.remove(0);
        List<List<Field>> boards = prepareBoards(input);
        Stack<Integer> stack = prepareNumbersToPlay(firstLine);
        var game = boards;
        while (!stack.isEmpty()) {
            var currentNumber = stack.pop();
            game = mark(game, currentNumber);
            var winningBoard = checkWinCondition(game);
            if (winningBoard.isPresent()) {
                var score = calculateScore(winningBoard.get());
                System.out.println(score * currentNumber);
                break;
            }
        }
    }

    private static void second(final String name) throws IOException, URISyntaxException {
        URL resource = Four.class.getResource(name);
        assert resource != null;
        List<String[]> input = readFile(resource);
        var firstLine = input.remove(0);
        List<List<Field>> boards = prepareBoards(input);
        Stack<Integer> stack = prepareNumbersToPlay(firstLine);
        var game = boards;
        var lastWinningScore = 0;
        while (!stack.isEmpty()) {
            var currentNumber = stack.pop();
            game = mark(game, currentNumber);
            var winningBoards = getAllWinningBoards(game);
            if (!winningBoards.isEmpty()) {
                for (List<Field> winner : winningBoards) {
                    game.removeIf(boardEquals(winner));
                    lastWinningScore = calculateScore(winner) * currentNumber;
                }
                if (game.isEmpty()) {
                    break;
                }
            }
        }
        System.out.println(lastWinningScore);
    }

    private static Predicate<? super List<Field>> boardEquals(final List<Field> winningBoard) {
        return winningBoard::equals;
    }

    private static List<String[]> readFile(final URL resource) throws IOException, URISyntaxException {
        return Files.lines(Paths.get(resource.toURI()))
                .filter(ss -> !ss.isBlank())
                .map(ss -> ss.split("(\\s)+"))
                .collect(Collectors.toList());
    }

    private static Stack<Integer> prepareNumbersToPlay(final String[] firstLine) {
        var split = Stream.of(firstLine[0].split(",")).map(Integer::parseInt).collect(Collectors.toList());
        var stack = new Stack<Integer>();
        Collections.reverse(split);
        stack.addAll(split);
        return stack;
    }

    private static List<List<Field>> prepareBoards(final List<String[]> input) {
        var i = 1;
        List<Field> currentBoard = new ArrayList<>();
        List<List<Field>> boards = new ArrayList<>();
        for (String[] l : input) {
            var line = Stream.of(l)
                    .filter(ss -> !ss.isEmpty())
                    .map(Integer::parseInt)
                    .map(ll -> new Field(ll, false))
                    .collect(Collectors.toList());
            currentBoard.addAll(line);
            if (i++ % 5 == 0) {
                boards.add(currentBoard);
                currentBoard = new ArrayList<>();
            }
        }
        return boards;
    }

    private static int calculateScore(final List<Field> winningBoard) {
        return winningBoard.stream().filter(b -> !b.getR()).mapToInt(Pair::getL).sum();
    }

    private static Optional<List<Field>> checkWinCondition(List<List<Field>> boards) {
        return boards.stream().filter(checkWinConditionBoard()).findFirst();
    }

    private static List<List<Field>> getAllWinningBoards(List<List<Field>> boards) {
        return boards.stream().filter(checkWinConditionBoard()).collect(Collectors.toList());
    }

    private static Predicate<? super List<Field>> checkWinConditionBoard() {
        return board -> {
            //Columns
            var columnResult = checkIfBoardWins(board, n -> n / 5);
            if (columnResult.isPresent()) {
                return true;
            }
            //Rows
            var rowResult = checkIfBoardWins(board, n -> n % 5);
            return rowResult.isPresent();
        };
    }

    private static Optional<Stream<Field>> checkIfBoardWins(final List<Field> board,
            Function<Integer, Integer> grouper) {
        return IntStream.range(0, 25)
                .mapToObj(n -> new Field(grouper.apply(n), board.get(n).getR()))
                .collect(Collectors.groupingBy(Pair::getL))
                .values()
                .stream()
                .map(List::stream)
                .filter(s -> s.map(Field::getR).allMatch(Predicate.isEqual(true)))
                .findAny();
    }

    private static List<List<Field>> mark(List<List<Field>> boards, final Integer currentNumber) {
        return boards.stream().map(markInBoard(currentNumber)).collect(Collectors.toList());
    }

    private static Function<List<Field>, List<Field>> markInBoard(final Integer currentNumber) {
        return board -> board.stream().map(f -> {
            if (f.getL() == currentNumber) {
                return new Field(f.getL(), true);
            } else {
                return f;
            }
        }).collect(Collectors.toList());
    }

    public static class Field extends Pair<Integer, Boolean> {

        public Field(final Integer integer, final Boolean aBoolean) {
            super(integer, aBoolean);
        }
    }
}
