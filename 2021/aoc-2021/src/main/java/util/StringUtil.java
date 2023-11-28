package util;

import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Stream;

public class StringUtil {

    public static Function<String, Stream<String>> split(String regex){
        return s -> Arrays.stream(s.split(regex));
    }
}
