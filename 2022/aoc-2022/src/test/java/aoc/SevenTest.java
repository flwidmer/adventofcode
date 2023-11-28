package aoc;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;

class SevenTest {
    public static final String TEST_FILE = "seven.test";
    public static Function<String, Seven> SUPPLIER = Seven::new;

    Seven underTest() {
        return SUPPLIER.apply(TEST_FILE);
    }

    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        assertThat(underTest().first()).isEqualTo(95437);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        assertThat(underTest().second()).isEqualTo("MCD");
    }
}
