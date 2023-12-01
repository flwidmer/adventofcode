package org.spick;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.URISyntaxException;

import static org.assertj.core.api.Assertions.assertThat;


class OneTest {
    @Test
    void shouldReturnTestResultOne() throws IOException, URISyntaxException {
        Assertions.assertEquals(new One("one.test").first(), 142);
    }

    @Test
    void shouldReturnTestResultTwo() throws IOException, URISyntaxException {
        Assertions.assertEquals(new One("one_2.test").second(), 281);
    }

    @Test
    void shouldReplaceAllNumbers() {
        var in = "eightwothree";
        var underTest = new One("");
        assertThat(underTest.replacerFunction(in))
                .contains("8")
                .contains("2")
                .contains("3");
    }
}
