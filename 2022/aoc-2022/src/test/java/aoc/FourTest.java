package aoc;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;

class FourTest {

    public static final String TEST_FILE = "four.test";
    public static Function<String, Four> SUPPLIER = Four::new;

    Four underTest() {
        return SUPPLIER.apply(TEST_FILE);
    }

    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        assertThat(underTest().first()).isEqualTo(2);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        assertThat(underTest().second()).isEqualTo(4);
    }

}
