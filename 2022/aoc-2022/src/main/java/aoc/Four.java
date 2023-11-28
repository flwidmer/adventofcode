package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Four extends AbstractPuzzle<Integer> {

  public static void main(String[] args) throws IOException, URISyntaxException {
    var puzzle = new Four("four.input");
    System.out.println(puzzle.first());
    System.out.println(puzzle.second());
  }

  public Four(String s) {
    super(s);
  }

  @Override
  public Integer first() throws IOException, URISyntaxException {
    return (int)
        Files.lines(getFilePath()).map(parseSectors()).filter(totalOverlapExists()).count();
  }

  @Override
  public Integer second() throws IOException, URISyntaxException {
    return (int) Files.lines(getFilePath()).map(parseSectors()).filter(overlapExists()).count();
  }

  private Predicate<? super PairAssignment> overlapExists() {
    return p ->
        p.firstSector.stream()
            .filter(area -> p.secondSector.contains(area))
            .findAny()
            .map(ignore -> true)
            .orElse(false);
  }

  private Predicate<PairAssignment> totalOverlapExists() {
    return p ->
        p.firstSector.containsAll(p.secondSector) || p.secondSector.containsAll(p.firstSector);
  }

  private Function<String, PairAssignment> parseSectors() {
    return s -> {
      var split = s.split("[-,]");
      return new PairAssignment(convertToSet(split[0], split[1]), convertToSet(split[2], split[3]));
    };
  }

  private static Set<Integer> convertToSet(final String begin, final String end) {
    return IntStream.rangeClosed(Integer.parseInt(begin), Integer.parseInt(end))
        .mapToObj(x -> x)
        .collect(Collectors.toSet());
  }

  record PairAssignment(Set<Integer> firstSector, Set<Integer> secondSector) {}
}
