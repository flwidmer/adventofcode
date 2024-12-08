package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestSeven {
     @Test
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(new Seven("seven.txt").first()).isEqualTo(3749);
    }

    @Test
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(new Seven("seven.txt").second()).isEqualTo(11387);
    }
}
