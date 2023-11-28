package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.List;
import java.util.Stack;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

public class Five extends AbstractPuzzle<String> {
  public Five(String s) {
    super(s);
  }

  public static void main(String[] args) throws IOException, URISyntaxException {
    var p = new Five("five.input");
    System.out.println(p.first());
    System.out.println(p.second());
  }

  @Override
  public String first() throws IOException, URISyntaxException {
    var input = parse();
    input.moves().stream().forEach(makeMoveCrateMover9000(input.state()));
    return input.state().stacks().stream().map(stack -> stack.peek()).collect(Collectors.joining());
  }

  @Override
  public String second() throws IOException, URISyntaxException {
    var input = parse();
    input.moves().stream().forEach(makeMoveCrateMover9001(input.state()));
    return input.state().stacks().stream().map(stack -> stack.peek()).collect(Collectors.joining());
  }

  private Consumer<? super Move> makeMoveCrateMover9000(State state) {
    return move -> {
      var origin = state.stacks().get(move.origin());
      var destination = state.stacks().get(move.destination());
      IntStream.range(0, move.amount())
          .mapToObj(ignore -> origin.pop())
          .forEach(item -> destination.push(item));
    };
  }

  private Consumer<? super Move> makeMoveCrateMover9001(State state) {
    return move -> {
      var origin = state.stacks().get(move.origin());
      var destination = state.stacks().get(move.destination());
      var temp = new Stack<String>();
      IntStream.range(0, move.amount())
          .mapToObj(ignore -> origin.pop())
          .forEach(item -> temp.push(item));
      while (!temp.isEmpty()) {
        destination.push(temp.pop());
      }
    };
  }

  private Input parse() throws URISyntaxException, IOException {
    var raw = Files.readString(getFilePath()).split("\r\n\r\n");
    return new Input(parseState(raw[0]), parseMoves(raw[1]));
  }

  private List<Move> parseMoves(String movesRaw) {
    // move 1 from 1 to 2
    return movesRaw
        .lines()
        .map(s -> s.split(" "))
        .map(split -> Stream.of(split[1], split[3], split[5]).mapToInt(Integer::parseInt))
        .map(stream -> stream.iterator())
        .map(iter -> new Move(iter.next(), iter.next() - 1, iter.next() - 1))
        .collect(Collectors.toList());
  }

  private State parseState(String stateRaw) {
    // find last line
    var revertedLines = stateRaw.lines().collect(Collectors.toList()).reversed();
    // split by space
    var size = revertedLines.removeFirst().split("\s{3}").length;
    var stacks =
        IntStream.range(0, size)
            .mapToObj(ignore -> new Stack<String>())
            .collect(Collectors.toList());
    // parse rest
    revertedLines.forEach(
        l -> {
          // take 4 approach
          var itemsRaw =
              Stream.of(splitStringIntoPiecesOf(l, "4"))
                  .map(s -> s.replaceAll("\s{4}", ""))
                  .map(s -> s.replaceAll("[\s\\[\\]]", ""));

          var stackIter = stacks.iterator();
          var itemIterator = itemsRaw.iterator();
          while (stackIter.hasNext() && itemIterator.hasNext()) {
            stackIter.next().push(itemIterator.next());
          }
        });
    stacks.forEach(
        st -> {
          while (!st.isEmpty() && st.peek().isBlank()) {
            st.pop();
          }
        });
    return new State(stacks);
  }

  private static String[] splitStringIntoPiecesOf(String string, final String pieceSize) {
    // TODO extract
    return string.split("(?<=\\G.{" + pieceSize + "})");
  }

  record Input(State state, List<Move> moves) {}

  record State(List<Stack<String>> stacks) {
    @Override
    public String toString() {
      return stacks.stream()
          .map(s -> s.stream().collect(Collectors.joining(",")))
          .collect(Collectors.joining("\n"));
    }
  }

  record Move(int amount, int origin, int destination) {
    @Override
    public String toString() {
      return "amount=" + amount + ", origin=" + origin + ", destination=" + destination;
    }
  }
}
