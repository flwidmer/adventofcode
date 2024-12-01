package org.spick.utils;

import java.util.Iterator;
import java.util.Stack;
import java.util.function.BiFunction;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class Zip<T> {

    private Iterator<T> left;
    private Iterator<T> right;

    public Zip(Stream<T> left, Stream<T> right) {
        this.left = left.iterator();
        this.right = right.iterator();
    }

    public <K> Stream<K> map(BiFunction<T, T, K> mappingFunction) {
        Iterable<K> resultIter = () -> new Iterator<>() {

            @Override
            public boolean hasNext() {
                return left.hasNext() && right.hasNext();
            }

            @Override
            public K next() {
                return mappingFunction.apply(left.next(), right.next());
            }

        };
        return StreamSupport.stream(resultIter.spliterator(), false);
    }
}
