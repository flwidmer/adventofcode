package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;

public class ThreeBruteForce extends AbstractPuzzle<Integer> {
    public ThreeBruteForce(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new ThreeBruteForce("three.input");
        System.out.println(puzzle.first());
    }

    @Override
    public Integer first() {
        var lines = streamLines()
                .map(String::toCharArray)
                .toList();
        var sum = 0;
        for (int i = 0; i < lines.size(); i++) {
            var matching = false;
            var symbol = false;
            String currentNumber = "";
            var currentLine = lines.get(i);
            var previousLine = i == 0 ? null : lines.get(i - 1);
            var nextLine = i + 1 < lines.size() ? lines.get(i + 1) : null;
            var length = lines.get(0).length;
            for (int k = 0; k < length; k++) {
                var currentChar = currentLine[k];
                if (isDigit(currentChar)) {
                    matching = true;
                    currentNumber = currentNumber + currentChar;
                    // check left
                    if (k != 0 && isSymbol(currentLine[k - 1])) {
                        symbol = true;
                    }
                    // check right
                    if (k + 1 < length && isSymbol(currentLine[k + 1])) {
                        symbol = true;
                    }
                    // check up diagonal
                    if (k != 0 && previousLine != null && isSymbol(previousLine[k - 1])) {
                        symbol = true;
                    }
                    if (k + 1 < length && previousLine != null && isSymbol(previousLine[k + 1])) {
                        symbol = true;
                    }
                    // check up direct
                    if (previousLine != null && isSymbol(previousLine[k])) {
                        symbol = true;
                    }
                    // check down diagonal
                    if (k != 0 && nextLine != null && isSymbol(nextLine[k - 1])) {
                        symbol = true;
                    }
                    if (k + 1 < length && nextLine != null && isSymbol(nextLine[k + 1])) {
                        symbol = true;
                    }
                    // check down direct
                    if (nextLine != null && isSymbol(nextLine[k])) {
                        symbol = true;
                    }

                } else if (!isDigit(currentChar) && matching) {
                    matching = false;
                    if (symbol && !currentNumber.isEmpty()) {
                        sum += Integer.parseInt(currentNumber);
                    }
                    currentNumber = "";
                    symbol = false;
                }
                if (k + 1 == length && symbol && !currentNumber.isEmpty()) {
                    sum += Integer.parseInt(currentNumber);
                }
            }
        }
        return sum;
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        return null;
    }

    private boolean isDigit(char c) {
        return 48 <= c && c <= 57;
    }

    private boolean isSymbol(char c) {
        return !isDigit(c) && c != '.';
    }
}
