package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

public class Prep extends AbstractPuzzle<Integer> {

    public Prep(String s) {
        super(s);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        var puzzle = new Prep("prep.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    @Override
    public Integer first() {
        return null;
    }

    @Override
    public Integer second() {
        return null;
    }
}
