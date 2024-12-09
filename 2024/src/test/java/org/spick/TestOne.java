package org.spick;

import org.assertj.core.api.Assertions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class TestOne {
    private One underTest;

    @BeforeEach
    public void setup() {
        underTest = new One("one.txt");
    }

    @Test
    public void testFirst() {
        Assertions.assertThat(underTest.first()).isEqualTo(11);
    }

    @Test
    public void testSecond() {
        Assertions.assertThat(underTest.second()).isEqualTo(31);
    }
}
