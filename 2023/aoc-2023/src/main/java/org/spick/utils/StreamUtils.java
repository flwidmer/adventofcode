package org.spick.utils;

import java.util.Map;
import java.util.stream.Collector;
import java.util.stream.Collectors;

public class StreamUtils {

    private StreamUtils() {
        //NOP
    }

    public static Collector<Map.Entry<String, Integer>, ?, Map<String, Integer>> collectEntriesIntoMap() {
        return Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue);
    }
}
