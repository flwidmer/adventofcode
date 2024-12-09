package org.spick;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class TestTwo {
    private Two underTest;

    @BeforeEach
    public void setup() {
        underTest = new Two("two.txt");
    }

    @Test
    public void testFirst() {
        Assertions.assertThat(underTest.first()).isEqualTo(2);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(underTest.second()).isEqualTo(4);
    }

    @Test
    public void testSecondLevelAtBeginning() {
        Assertions.assertThat(new Two("two_a.txt").second()).isEqualTo(4);
    }
}
