package org.spick;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyStore.Entry;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class Five extends AbstractPuzzle<Integer> {

    public Five(String input) {
        super(input);
    }

    public static void main(String[] args) throws IOException, URISyntaxException {
        Five five = new Five("five.txt");
        System.out.println(five.first());
        System.out.println(five.second());
    }

    @Override
    public Integer first() throws IOException, URISyntaxException {
        var result = 0;
        var rulesBackward = getRulesBackward();
        for (var manual : getManuals()) {
            if (isOkay(manual, rulesBackward)) {
                result += manual.get(manual.size() / 2);
            }
        }
        return result;
    }

    @Override
    public Integer second() throws IOException, URISyntaxException {
        var result = 0;
        var rulesBackward = getRulesBackward();
        var rulesForward = getRulesForward();
        for (var manual : getManuals()) {
            if (!isOkay(manual, rulesBackward)) {
                manual = fix(manual, rulesForward);
                result += manual.get(manual.size() / 2);
            }
        }
        return result;
    }

    private List<Integer> fix(List<Integer> manual, Map<Integer, Set<Integer>> rulesForward) {
        var precedingPages = new ArrayList<Integer>();
        for (var i = 0; i <  manual.size(); i++) {
            var page = manual.get(i);
            if (!rulesForward.containsKey(page)) {
                precedingPages.add(page);
                continue;
            }
            var lowestIndex = manual.size();
            for (var rule : rulesForward.get(page)) {
                if(precedingPages.isEmpty()) {
                    break;
                } else if(precedingPages.contains(rule)){
                    var indexOfRule = precedingPages.indexOf(rule);
                    if(indexOfRule < lowestIndex && indexOfRule != -1) {
                        lowestIndex = indexOfRule;
                    }
                }
            }
            if(lowestIndex != manual.size()) {
                precedingPages.add(lowestIndex, page);
            } else {
                precedingPages.add(page);
            }
        }
        return precedingPages;
    }

    private List<List<Integer>> getManuals() {
        return streamLines().dropWhile(line -> !line.contains(","))
                .map(line -> line.split(","))
                .map(array -> Arrays.stream(array)
                        .map(Integer::parseInt)
                        .toList())
                .toList();
    }

    private boolean isOkay(List<Integer> manual, Map<Integer, Set<Integer>> rules) {
        var precedingPages = new HashSet<Integer>();
        var containedPages = new HashSet<>(manual);
        for (var page : manual) {
            if (!rules.containsKey(page)) {
                precedingPages.add(page);
                continue;
            }
            for (var rule : rules.get(page)) {
                if (precedingPages.contains(rule) || !containedPages.contains(rule)) {
                    precedingPages.add(page);
                } else {
                    return false;
                }
            }
            ;
        }
        return true;
    }

    private Map<Integer, Set<Integer>> getRulesBackward() {
        var result = new HashMap<Integer, Set<Integer>>();
        streamLines()
                .filter(line -> line.contains("|"))
                .map(line -> line.split("\\|"))
                .collect(Collectors.groupingBy(pair -> Integer.parseInt(pair[1])))
                .entrySet()
                .stream()
                .map(entry -> Map.entry(entry.getKey(),
                        entry.getValue().stream()
                                .map(rule -> Integer.parseInt(rule[0]))
                                .collect(Collectors.toSet())))
                .forEach(e -> result.put(e.getKey(), e.getValue()));
        return result;
    }

    private Map<Integer, Set<Integer>> getRulesForward() {
        var result = new HashMap<Integer, Set<Integer>>();
        streamLines()
                .filter(line -> line.contains("|"))
                .map(line -> line.split("\\|"))
                .collect(Collectors.groupingBy(pair -> Integer.parseInt(pair[0])))
                .entrySet()
                .stream()
                .map(entry -> Map.entry(entry.getKey(),
                        entry.getValue().stream()
                                .map(rule -> Integer.parseInt(rule[1]))
                                .collect(Collectors.toSet())))
                .forEach(e -> result.put(e.getKey(), e.getValue()));
        return result;
    }
}
