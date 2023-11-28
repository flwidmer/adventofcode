package aoc;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;

class OneTest {

    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        Assertions.assertEquals(new One("one.test").first(), 24000);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        Assertions.assertEquals(new One("one.test").second(), 45000);
    }

}
