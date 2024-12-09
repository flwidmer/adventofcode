package org.spick.utils;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Stream;

public class FileInput implements Input {
    protected String inputFile;
    private Path inputPath;

    public FileInput(String s) {
        this.inputFile = s;
    }

    public FileInput(Path inputPath) {
        this.inputPath = inputPath;
    }

    @Override
    public Stream<String> streamLines() {
        try {
            return Files.lines(getFilePath());
        } catch (IOException | URISyntaxException e) {
            throw new RuntimeException("fumbled the bag", e);
        }
    }

    @Override
    public String readInput() {
        try {
            return Files.readString(getFilePath());
        } catch (IOException | URISyntaxException e) {
            throw new RuntimeException("fumbled the bag", e);
        }
    }

    protected Path getFilePath() throws URISyntaxException {
        if (this.inputPath != null) {
            return this.inputPath;
        }
        return Path.of(getClass().getResource(this.inputFile).toURI());
    }
}
