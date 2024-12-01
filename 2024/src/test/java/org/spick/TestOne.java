package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.spick.One;

public class TestOne {
    private One underTest;

    @BeforeEach
    public void setup() {
        underTest = new One("one.txt");
    }

    @Test 
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(underTest.first()).isEqualTo(11);
    }

    @Test 
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(underTest.second()).isEqualTo(31);
    }
}
