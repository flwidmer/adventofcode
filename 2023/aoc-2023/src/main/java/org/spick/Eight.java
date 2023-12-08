package org.spick;

import org.spick.utils.MathUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Eight extends AbstractPuzzle<Long> {

    public static final Pattern NODE_PATTERN = Pattern.compile("([A-Z0-9]{3}) = \\(([A-Z0-9]{3}), ([A-Z0-9]{3})\\)");

    public Eight(String s) {
        super(s);
    }

    public static void main(String[] args) {
        var puzzle = new Eight("eight.input");
        System.out.println(puzzle.first());
        System.out.println(puzzle.second());
    }

    //16897
    @Override
    public Long first() {
        var instructions = streamLines()
                .map(parseInstructions())
                .limit(1)
                .findFirst().orElseThrow();

        var mappedTree = streamLines().skip(2)
                .map(parseTreeNode())
                .collect(Collectors.toMap(TreeNode::name, Function.identity()));
        var currentNode = "AAA";
        var stepCount = 0;
        var instructionPointer = 0;
        while (!"ZZZ".equals(currentNode)) {
            stepCount++;
            var instruction = instructions.get(instructionPointer);
            currentNode = mappedTree.get(currentNode).get(instruction);
            instructionPointer = (instructionPointer + 1) % instructions.size();
        }
        return (long) stepCount;
    }

    //16563603485021
    @Override
    public Long second() {
        var instructions = streamLines()
                .map(parseInstructions())
                .limit(1)
                .findFirst().orElseThrow();
        var mappedTree = streamLines().skip(2)
                .map(parseTreeNode())
                .collect(Collectors.toMap(TreeNode::name, Function.identity()));
        var currentNodes = mappedTree.keySet().stream()
                .filter(s -> s.endsWith("A"))
                .toArray(String[]::new);
        var stepCount = 0;
        var instructionPointer = 0;
        var cycleContents = IntStream.range(0, currentNodes.length)
                .mapToObj(x -> new ArrayList<>())
                .toList();
        var cycles = IntStream.range(0, currentNodes.length)
                .map(x -> -1)
                .toArray();
        while (Arrays.stream(cycles).anyMatch(i -> i == -1)) {
            stepCount++;
            var instruction = instructions.get(instructionPointer);
            for (int i = 0; i < currentNodes.length; i++) {
                var nextNode = mappedTree.get(currentNodes[i]).get(instruction);
                if (cycles[i] == -1) { //Not yet finished
                    if (nextNode.endsWith("Z")) {
                        cycles[i] = stepCount;
                    }
                    cycleContents.get(i).add(currentNodes[i]); // Only needed for debug and understanding
                    currentNodes[i] = nextNode;
                }
            }
            instructionPointer = (instructionPointer + 1) % instructions.size();
        }

        return Arrays.stream(cycles)
                .mapToLong(i -> i)
                .reduce(1, MathUtils::lcm);
    }

    private Function<? super String, TreeNode> parseTreeNode() {
        //AAA = (BBB, CCC)
        return s -> {
            var matcher = NODE_PATTERN.matcher(s);
            if (!matcher.matches()) {
                panic("panic2!");
            }
            return new TreeNode(matcher.group(1), matcher.group(2), matcher.group(3));
        };
    }

    private Function<? super String, List<String>> parseInstructions() {
        // Makes an arraylist underneath
        return s -> Arrays.asList(s.split(""));
    }

    public record TreeNode(String name, String left, String right) {

        public String get(String instr) {
            return switch (instr) {
                case "L" -> left();
                case "R" -> right();
                default -> panic("panic!");
            };
        }
    }


}
