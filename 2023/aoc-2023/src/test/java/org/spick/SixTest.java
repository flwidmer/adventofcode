package org.spick;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SixTest {


    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        assertEquals(new Six("six.test").first(), 288);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        assertEquals(new Six("six.test").second(), 71503);
    }

}
