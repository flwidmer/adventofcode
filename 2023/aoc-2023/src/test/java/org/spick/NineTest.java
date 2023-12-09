package org.spick;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class NineTest {

    @Test
    void shouldReturnTestResultOne() {
        assertEquals(new Nine("nine.test").first(), 114);
    }

    @Test
    void shouldReturnTestResultTwo() {
        assertEquals(new Nine("nine.test").second(), 2);
    }

}
