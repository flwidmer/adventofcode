package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFour {
    @Test
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(new Four("four.txt").first()).isEqualTo(18);
    }

    @Test
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(new Four("four.txt").second()).isEqualTo(9);
    }
}
