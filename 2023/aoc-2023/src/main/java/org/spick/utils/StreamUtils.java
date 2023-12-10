package org.spick.utils;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StreamUtils {

    private StreamUtils() {
        //NOP
    }

    public static Collector<Map.Entry<String, Integer>, ?, Map<String, Integer>> collectEntriesIntoMap() {
        return Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue);
    }

    public static List<Long> parseListLong(String string) {
        Objects.requireNonNull(string);
        return Stream.of(string.split(" "))
                .mapToLong(Long::parseLong)
                .boxed()
                .toList();
    }
}
