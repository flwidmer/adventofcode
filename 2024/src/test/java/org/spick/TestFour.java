package org.spick;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFour {
    @Test
    public void testFirst() {
        Assertions.assertThat(new Four("four.txt").first()).isEqualTo(18);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(new Four("four.txt").second()).isEqualTo(9);
    }
}
