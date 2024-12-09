package org.spick;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestSeven {
    @Test
    public void testFirst() {
        Assertions.assertThat(new Seven("seven.txt").first()).isEqualTo(3749);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(new Seven("seven.txt").second()).isEqualTo(11387);
    }
}
