package org.spick;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

class SevenTest {
    @Test
    void shouldReturnTestResultOne() {
        assertEquals(new Seven("seven.input").first(), 6440L);
    }

    @Test
    void shouldReturnTestResultTwo() {
        assertEquals(new Seven("seven.input").second(), 5905L);
    }

    @Test
    void shouldSplitString() {
        var underTest = "ABC";
        assertThat(Arrays.stream(underTest.split("")).toList()).containsExactly("A", "B", "C");
    }

    @Test
    void shouldOrder() {
        var given = new ArrayList<>(List.of("2", "3", "4", "5", "A", "K", "J"));
        given.sort(new Seven("").cardOrderComparator());
        assertThat(given).containsExactly("A", "K", "J", "5", "4", "3", "2");
    }
}
