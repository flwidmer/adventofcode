package org.spick.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.net.Authenticator;
import java.net.CookieStore;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Properties;
import java.util.stream.Stream;

public class InputDownloader implements Input {

    private static final String COOKIE_NAME = "session";
    private static final String FILENAME = "token.cfg";
    private int day;
    private Properties properties;
    private String content;

    public InputDownloader(int day) {
        this.day = day;
        loadProperties();
    }

    public Stream<String> streamLines() {
        return new BufferedReader(new StringReader(readInput())).lines();
    }

    public String readInput() {
        if (content == null) {
            try {
                var request = HttpRequest.newBuilder(new URI("https://adventofcode.com/2024/day/" + day + "/input"))
                        .header("Cookie", COOKIE_NAME + "=" + properties.getProperty(COOKIE_NAME))
                        .GET()
                        .build();
                var response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
                content = response.body();
            } catch (URISyntaxException | IOException | InterruptedException e) {
                throw new RuntimeException("fumbled the bag", e);
            }
        }
        return content;
    }

    private void loadProperties() {

        try {
            properties = new Properties();
            Path path = Path.of(getClass().getResource(FILENAME).toURI());
            properties.load(Files.newBufferedReader(path));
        } catch (URISyntaxException | IOException e) {
            throw new RuntimeException("fumbled the bag", e);
        }

    }
}
