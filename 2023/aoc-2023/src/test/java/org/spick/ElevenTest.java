package org.spick;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ElevenTest {

    @Test
    void shouldReturnTestResultOne() {
        assertEquals(new Eleven("eleven.test").first(), 374);
    }

    @Test
    void shouldReturnTestResultTwo() {
        assertEquals(new Eleven("eleven.test").second(), 82000210);
    }

}
