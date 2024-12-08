package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

public class TestEight {
      @Test
    public void testFirst() throws IOException, URISyntaxException {
        Assertions.assertThat(new Eight("eight.txt").first()).isEqualTo(14);
    }


    @Test
    public void testInput() {
        var map = new Eight("eight_result.txt").getInput();
        map.antennas().stream().filter(x -> x.frequency() == '#')
        .forEach(x -> System.out.println(x.location()));

    }

    @Test
    public void testSecond() throws IOException, URISyntaxException {
        Assertions.assertThat(new Eight("eight.txt").second()).isEqualTo(34);
    }
}
