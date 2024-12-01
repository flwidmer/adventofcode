package org.spick.utils;

import java.util.ArrayDeque;
import java.util.Collection;
import java.util.Iterator;
import java.util.Queue;
import java.util.Spliterator;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class SlidingWindow<T> implements Spliterator<Stream<T>> {

    private final Iterator<T> sourceIterator;
    private final Queue<T> buffer;
    private final int windowSize;
    private final Collection<T> source;

    public static <T> Stream<Stream<T>> windowed(Collection<T> source, int windowSize) {
        return StreamSupport.stream(new SlidingWindow<>(source, windowSize), false);
    }

    private SlidingWindow(Collection<T> source, int windowSize) {
        this.source = source;
        this.sourceIterator = source.iterator();
        this.windowSize = windowSize;
        this.buffer = new ArrayDeque<>(windowSize);
    }

    @Override
    public boolean tryAdvance(Consumer<? super Stream<T>> action) {
        if (windowSize < 1) {
            return false;
        }
        while (sourceIterator.hasNext()) {
            buffer.add(sourceIterator.next());
            if (buffer.size() == windowSize) {
                action.accept(buffer.stream());
                buffer.poll();
                return true;
            }
        }
        return false;
    }

    @Override
    public Spliterator<Stream<T>> trySplit() {
        return null;
    }

    @Override
    public long estimateSize() {
        if (source.size() < windowSize) {
            return 0;
        }
        return source.size() - windowSize + 1L;
    }

    @Override
    public int characteristics() {
        return ORDERED | NONNULL | SIZED;
    }
}
