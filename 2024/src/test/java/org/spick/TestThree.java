package org.spick;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestThree {

    @Test
    public void testFirst() {
        Assertions.assertThat(new Three("three.txt").first()).isEqualTo(161);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(new Three("three_a.txt").second()).isEqualTo(48);
    }
}