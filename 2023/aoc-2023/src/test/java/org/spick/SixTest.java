package org.spick;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SixTest {


    @Test
    void shouldReturnTestResultOneBrute() {
        assertEquals(new Six("six.test").firstBruteForce(), 288);
    }

    @Test
    void shouldReturnTestResultTwoBrute() {
        assertEquals(new Six("six.test").secondBruteForce(), 71503);
    }

    @Test
    void shouldReturnTestResultOne() {
        assertEquals(new Six("six.test").first(), 288);
    }

    @Test
    void shouldReturnTestResultTwo() {
        assertEquals(new Six("six.test").second(), 71503);
    }

}
