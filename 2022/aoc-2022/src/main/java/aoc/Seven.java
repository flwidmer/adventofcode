package aoc;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class Seven extends AbstractPuzzle<Integer> {
    public Seven(String s) {
        super(s);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        var p = new Seven("seven.input");
        System.out.println(p.first());
        System.out.println(p.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        var slash = parseFileSystem();
        var sizeMap = new HashMap<String, Integer>();
        visit(slash, sizeMap);
        return sizeMap.values()
                .stream()
                .filter(x -> x <= 100000)
                .mapToInt(x -> x)
                .sum();
    }

    private void visit(FsDirectory current, HashMap<String, Integer> sizeMap) {
        var children = current.items.values();
        var mySize = 0;
        for (FsItem c : children) {
            switch (c) {
                case FsDirectory(Map<String, FsItem> _, String name, FsDirectory _):
                    visit((FsDirectory) c, sizeMap);
                    mySize += sizeMap.get(name);
                    break;
                case FsFile(int size, String _):
                    mySize += size;
                    break;
                default:
                    throw new IllegalStateException("Unexpected Type: " + c);
            }
        }
        sizeMap.put(current.name(), mySize);
    }

    private FsDirectory parseFileSystem() throws URISyntaxException, IOException {
        var input = Files.readAllLines(getFilePath());
        var parser = new Parser(input);
        parser.beginParse();
        return parser.getSlash();
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        return null;
    }

    interface FsItem {
        String name();
    }

    record FsDirectory(Map<String, FsItem> items, String name, FsDirectory parent) implements FsItem {
    }

    record FsFile(int size, String name) implements FsItem {
    }

    private class Parser {
        private FsDirectory currentDirectory;
        private FsDirectory slash;
        private Stack<String> input;

        public Parser(final List<String> input) {
            this.input = new Stack<>();
            input.reversed().forEach(this.input::push);
        }

        public FsDirectory getSlash() {
            return slash;
        }

        public void beginParse() {
            slash = newDirectory("/", null);
            currentDirectory = slash;
            var slashExpected = nextToken();
            expect(slashExpected, "$ cd /");
            parseCd("/");
            while (hasNext()) {
                var nextDir = nextToken();
                if (nextDir.equals("$ cd ..")) {
                    upOneDirectory();
                } else {
                    var name = nextDir.split(" ")[2];
                    if (currentDirectory.items().get(name) instanceof FsDirectory dir) {
                        currentDirectory = dir;
                    } else {
                        throw new RuntimeException("cd to file");
                    }
                    parseCd(name);
                }
            }
        }

        private void upOneDirectory() {
            currentDirectory = currentDirectory.parent;
        }


        private void parseCd(String name) {
            var lsExpected = nextToken();
            expect(lsExpected, "$ ls");
            while (hasNext() && !peekToken().startsWith("$")) {
                var content = nextToken();
                if (content.startsWith("dir")) {
                    var dir = parseDir(content);
                    putInCurrent(dir);
                } else if (content.matches("\\d+ .+")) {
                    var file = parseFile(content);
                    putInCurrent(file);
                }
            }
        }

        private boolean hasNext() {
            return !this.input.isEmpty();
        }

        private void putInCurrent(FsItem file) {
            currentDirectory.items().put(file.name(), file);
        }

        private FsFile parseFile(String content) {
            var s = content.split(" ");
            return new FsFile(Integer.parseInt(s[0]), s[1]);
        }

        private FsDirectory parseDir(String content) {
            var s = content.split("dir ");
            return newDirectory(s[1], currentDirectory);
        }

        private String nextToken() {
            return this.input.pop();
        }

        private String peekToken() {
            return this.input.peek();
        }

        private static FsDirectory newDirectory(final String name, FsDirectory parent) {
            return new FsDirectory(new HashMap<>(), name, parent);
        }

        private static void expect(String inThisString, String iExpect) {
            if (!inThisString.equals(iExpect)) {
                throw new RuntimeException(STR. "expecting \{ inThisString }" );
            }
        }
    }
}
