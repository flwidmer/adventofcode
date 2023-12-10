package org.spick;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class TenTest {
    @Test
    void shouldReturnTestResultOne() {
        assertEquals(new Ten("ten.test").first(), 8);
    }

    @Test
    void shouldReturnTestResultOneBfs() {
        assertEquals(new Ten("ten.test").firstBfs(), 8);
    }

    @Test
    void shouldReturnTestResultTwo() {
        assertEquals(new Ten("ten.test").second(), 123);
    }
}
