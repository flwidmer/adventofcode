package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.function.Function;
import java.util.function.ToIntFunction;


public class Two extends AbstractPuzzle<Integer> {

    public static void main(String[] args) throws IOException, URISyntaxException {
        var two = new Two("two.input");
        System.out.println(two.first());
        System.out.println(two.second());
    }

    public Two(String s) {
        super(s);
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        return Files.readAllLines(getFilePath())
                .stream()
                .map(parseFirst())
                .mapToInt(evaluate())
                .sum();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        return Files.readAllLines(getFilePath())
                .stream()
                .map(parseSecond())
                .mapToInt(evaluate())
                .sum();
    }

    private ToIntFunction<Game> evaluate() {
        return g -> {
            var win = switch (g.opp) {
                case ROCK -> switch (g.mine) {
                    case ROCK -> 3;
                    case PAPER -> 6;
                    case SCISSOR -> 0;
                };
                case PAPER -> switch (g.mine) {
                    case ROCK -> 0;
                    case PAPER -> 3;
                    case SCISSOR -> 6;
                };
                case SCISSOR -> switch (g.mine) {
                    case ROCK -> 6;
                    case PAPER -> 0;
                    case SCISSOR -> 3;
                };
            };
            return win + g.mine().getValue();
        };
    }

    private Function<String, Game> parseFirst() {
        return s -> {
            var split = s.split(" ");
            var opp = switch (split[0]) {
                case "A" -> Choice.ROCK;
                case "B" -> Choice.PAPER;
                case "C" -> Choice.SCISSOR;
                default -> null;
            };
            var mine = switch (split[1]) {
                case "X" -> Choice.ROCK;
                case "Y" -> Choice.PAPER;
                case "Z" -> Choice.SCISSOR;
                default -> null;
            };
            return new Game(opp, mine);
        };
    }

    private Function<String, Game> parseSecond() {
        return s -> {
            var split = s.split(" ");
            var opp = switch (split[0]) {
                case "A" -> Choice.ROCK;
                case "B" -> Choice.PAPER;
                case "C" -> Choice.SCISSOR;
                default -> null;
            };
            var mine = switch (split[1]) {
                case "X" -> switch (opp){
                    case Choice.ROCK -> Choice.SCISSOR;
                    case Choice.PAPER -> Choice.ROCK;
                    case Choice.SCISSOR -> Choice.PAPER;
                };
                case "Y" -> opp;
                case "Z" -> switch (opp){
                    case Choice.ROCK -> Choice.PAPER;
                    case Choice.PAPER -> Choice.SCISSOR;
                    case Choice.SCISSOR -> Choice.ROCK;
                };
                default -> null;
            };
            return new Game(opp, mine);
        };
    }

    record Game(Choice opp, Choice mine) {
    }


    enum Choice {
        ROCK(1),
        PAPER(2),
        SCISSOR(3);

        private final int value;

        Choice(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }
}
