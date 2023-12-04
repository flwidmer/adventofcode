package org.spick;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class FourTest {
    @Test
    void shouldReturnTestResultOne() {
        Assertions.assertEquals(new Four("four.test").first(), 13);
    }

    @Test
    void shouldReturnTestResultTwo() {
        Assertions.assertEquals(new Four("four.test").second(), 30);
    }
}
