package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestSix {

    @Test
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(new Six("six.txt").first()).isEqualTo(41);
    }

    @Test
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(new Six("six.txt").second()).isEqualTo(6);
    }

}
