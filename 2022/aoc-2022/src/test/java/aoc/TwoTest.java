package aoc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;

class TwoTest {
    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        Assertions.assertEquals(new Two("two.test").first(), 15);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        Assertions.assertEquals(new Two("two.test").second(), 12);
    }
}
