package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.stream.Collectors;

public class Six extends AbstractPuzzle<Integer> {
    public Six(String s) {
        super(s);
    }

    public Six(Path file) {
        super(file);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        var p = new Six("six.input");
        System.out.println(p.first());
        System.out.println(p.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        return calculateMessage(4);
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        return calculateMessage(14);
    }

    private int calculateMessage(final int size) throws IOException, URISyntaxException {
        var input = inputString();
        var inputIter = input.chars().mapToObj(x -> x).collect(Collectors.toList()).iterator();
        var buffer = initBuffer(inputIter, size);
        var counter = size;
        while (inputIter.hasNext()) {
            if (buffer.stream().collect(Collectors.toSet()).size() == size) {
                return counter;
            }
            buffer.removeFirst();
            buffer.addLast(inputIter.next());
            counter++;
        }
        return -1;
    }

    private LinkedList<Integer> initBuffer(Iterator<Integer> inputIter, int size) {
        final var buffer = new LinkedList<Integer>();
        for (int i = 0; i < size; i++) {
            buffer.addLast(inputIter.next());
        }
        return buffer;
    }

    private String inputString() throws IOException, URISyntaxException {
        return Files.readAllLines(getFilePath()).getFirst();
    }
}
