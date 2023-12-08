package org.spick;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

class EightTest {
    @Test
    void shouldReturnTestResultOne() {
        assertEquals(new Eight("eight.test").first(), 2L);
    }

    @Test
    void shouldReturnTestResultOne_2() {
        assertEquals(new Eight("eight_2.test").first(), 6L);
    }

    @Test
    void shouldReturnTestResultTwo() {
        assertEquals(new Eight("eight_3.test").second(), 6L);
    }

    @Test
    void shouldMatch() {
        var matcher = Eight.NODE_PATTERN.matcher("AAA = (BBB, CCC)");
        assertThat(matcher).matches();
    }

}
