package org.spick;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestSix {

    @Test
    public void testFirst() {
        Assertions.assertThat(new Six("six.txt").first()).isEqualTo(41);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(new Six("six.txt").second()).isEqualTo(6);
    }

}
