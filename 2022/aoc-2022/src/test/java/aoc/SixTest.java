package aoc;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.LinkedList;
import java.util.stream.Collectors;

import static org.assertj.core.api.Assertions.assertThat;

class SixTest {

    @ParameterizedTest
    @CsvSource("""
            bvwbjplbgvbhsrlpgdmjqwftvncz, 5
            mjqjpqmgbljsphdztnvjfqwrcgsmlb, 7
            nppdvjthqldpwncqszvftbrmjlhg, 6
            nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg, 10
            zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw, 11
            """)
    void shouldReturnTestResultOne(String input, Integer expected) throws IOException, URISyntaxException {
        var file = Files.createTempFile("test", "sixone");
        Files.writeString(file, input);
        assertThat(new Six(file).first()).isEqualTo(expected);
    }

    @ParameterizedTest
    @CsvSource("""
            mjqjpqmgbljsphdztnvjfqwrcgsmlb, 19
            bvwbjplbgvbhsrlpgdmjqwftvncz, 23
            nppdvjthqldpwncqszvftbrmjlhg, 23
            nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg, 29
            zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw, 26
            """)
    void shouldReturnTestResultTwo(String input, Integer expected) throws IOException, URISyntaxException {
        var file = Files.createTempFile("test", "sixtwo");
        Files.writeString(file, input);
        assertThat(new Six(file).second()).isEqualTo(expected);
    }
}
