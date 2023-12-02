package org.spick;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.regex.Pattern;

import static org.assertj.core.api.Assertions.assertThat;

class TwoTest {

    @Test
    void shouldReturnTestResultOne() {
        Assertions.assertEquals(new Two("two.test").first(), 8);
    }

    @Test
    void shouldReturnTestResultTwo() {
        Assertions.assertEquals(new Two("two.test").second(), 2286);
    }

    @Test
    void shouldMatch() {
        var pattern = Pattern.compile("(\\d+)\\s([A-z]+)");
        var matcher = pattern.matcher("20 red");
        assertThat(matcher.matches());
    }

}
