package org.spick;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.stream.IntStream;

import static org.assertj.core.api.Assertions.assertThat;

class ThreeTest {
    @Test
    void shouldReturnTestResultOne() {
        Assertions.assertEquals(new Three("three.test").first(), 4361);
    }


    @Test
    void shouldReturnTestResultTwo() {
        Assertions.assertEquals(new Three("three.test").second(), 467835);
    }

    @Test
    void shouldBeDigit() {
        IntStream.range(0, 10)
                .mapToObj(Integer::toString)
                .map(s -> s.charAt(0))
                .forEach(c -> assertThat(48 <= c && c <= 57).isTrue());
    }

    @Test
    void shouldBeWithin() {
        var range = new Three.NumberRange(4, 7);
        assertThat(range.touches(new Three.SymbolPosition(3, 'u'))).isTrue();
        assertThat(range.touches(new Three.SymbolPosition(4, 'u'))).isTrue();
        assertThat(range.touches(new Three.SymbolPosition(5, 'u'))).isTrue();
        assertThat(range.touches(new Three.SymbolPosition(6, 'u'))).isTrue();
        assertThat(range.touches(new Three.SymbolPosition(7, 'u'))).isTrue();
        assertThat(range.touches(new Three.SymbolPosition(8, 'u'))).isTrue();
        assertThat(range.touches(new Three.SymbolPosition(2, 'u'))).isFalse();
        assertThat(range.touches(new Three.SymbolPosition(9, 'u'))).isFalse();
    }
}
