package org.spick.utils;

import java.util.stream.Stream;

public interface Input {
    public Stream<String> streamLines();
    public String readInput();
}
