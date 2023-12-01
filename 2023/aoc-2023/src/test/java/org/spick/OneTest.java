package org.spick;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.junit.jupiter.api.Assertions.*;

class OneTest {
    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        Assertions.assertEquals(new One("one.test").first(), 142);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        Assertions.assertEquals(new One("one_2.test").second(), 281);
    }

}
