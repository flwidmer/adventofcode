package org.spick;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class PrepTest {

@Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        assertEquals(new Prep("one.test").first(), 123);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        assertEquals(new Prep("one.test").second(), 123);
    }
}
