package util;

import java.util.Objects;

public class Pair<L,R> {

    private final L l;
    private final R r;

    public Pair(L l, R r) {
        this.l = l;
        this.r = r;
    }

    public L getL() {
        return l;
    }

    public R getR() {
        return r;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final Pair<?, ?> pair = (Pair<?, ?>) o;
        return Objects.equals(l, pair.l) && Objects.equals(r, pair.r);
    }

    @Override
    public int hashCode() {
        return Objects.hash(l, r);
    }
}
