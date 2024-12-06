package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFive {

        @Test
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(new Five("five.txt").first()).isEqualTo(143);
    }

    @Test
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(new Five("five.txt").second()).isEqualTo(123);
    }
    
}
