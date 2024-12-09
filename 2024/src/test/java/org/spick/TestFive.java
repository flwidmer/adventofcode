package org.spick;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestFive {

    @Test
    public void testFirst() {
        Assertions.assertThat(new Five("five.txt").first()).isEqualTo(143);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(new Five("five.txt").second()).isEqualTo(123);
    }

}
