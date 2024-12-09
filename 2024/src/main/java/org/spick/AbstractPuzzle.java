package org.spick;

import java.nio.file.Path;
import java.util.stream.Stream;

import org.spick.utils.FileInput;
import org.spick.utils.Input;
import org.spick.utils.InputDownloader;

public abstract class AbstractPuzzle<RESULT_TYPE> {

    private Input input;

    public AbstractPuzzle(String s) {
        this.input = new FileInput(s);
    }

    public AbstractPuzzle(Path inputPath) {
        this.input = new FileInput(inputPath);
    }

    public AbstractPuzzle(int day) {
        this.input = new InputDownloader(day);
    }

    public abstract RESULT_TYPE first();

    protected Stream<String> streamLines() {
        return input.streamLines();
    }

    protected String readInput() {
       return input.readInput();
    }

    protected static <T> T panic(String s) {
        throw new RuntimeException(s);
    }

    public abstract RESULT_TYPE second();
}
