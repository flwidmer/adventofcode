package aoc;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;

class FiveTest {
    public static final String TEST_FILE = "five.test";
    public static Function<String, Five> SUPPLIER = Five::new;

    Five underTest() {
        return SUPPLIER.apply(TEST_FILE);
    }

    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        assertThat(underTest().first()).isEqualTo("CMZ");
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        assertThat(underTest().second()).isEqualTo("MCD");
    }

}
