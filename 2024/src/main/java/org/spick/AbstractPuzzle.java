package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

public abstract class AbstractPuzzle<RESULT_TYPE> {
  protected String inputFile;
  private Path inputPath;

  public AbstractPuzzle(String s) {
    this.inputFile = s;
  }

  public AbstractPuzzle(Path inputPath) {
    this.inputPath = inputPath;
  }

  public abstract RESULT_TYPE first() throws IOException, URISyntaxException;

  protected Stream<String> streamLines() {
    try {
      return Files.lines(getFilePath());
    } catch (IOException | URISyntaxException e) {
      throw new RuntimeException("fumbled the bag", e);
    }
  }

  protected String readInput() {
    try {
      return Files.readString(getFilePath());
    } catch (IOException | URISyntaxException e) {
      throw new RuntimeException("fumbled the bag", e);
    }
  }

  protected static <T> T panic(String s) {
    throw new RuntimeException(s);
  }

  public abstract RESULT_TYPE second() throws IOException, URISyntaxException;

  protected Path getFilePath() throws URISyntaxException {
    if (this.inputPath != null) {
      return this.inputPath;
    }
    return Path.of(getClass().getResource(this.inputFile).toURI());
  }
}
