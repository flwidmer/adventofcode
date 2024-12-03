package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestThree {

    @Test
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(new Three("three.txt").first()).isEqualTo(161);
    }

    @Test
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(new Three("three_a.txt").second()).isEqualTo(48);
    }
}