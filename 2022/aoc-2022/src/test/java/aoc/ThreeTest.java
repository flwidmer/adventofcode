package aoc;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.util.function.Function;

import static org.assertj.core.api.Assertions.assertThat;

class ThreeTest {
    public static final String TEST_FILE = "three.test";
    public static Function<String, Three> SUPPLIER = Three::new;

    Three underTest() {
        return SUPPLIER.apply(TEST_FILE);
    }

    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        assertThat(underTest().first()).isEqualTo(157);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        assertThat(underTest().second()).isEqualTo(70);
    }

    @Test
    void aShouldBe1() {
        assertThat(underTest().mapLetter('a')).isEqualTo(1);
    }

    @Test
    void AShouldBe27() {
        assertThat(underTest().mapLetter('A')).isEqualTo(27);
    }

}
