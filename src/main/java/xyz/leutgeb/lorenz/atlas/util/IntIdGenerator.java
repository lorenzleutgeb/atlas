package xyz.leutgeb.lorenz.atlas.util;

import java.util.Iterator;
import java.util.function.Supplier;

public class IntIdGenerator implements Iterator<Integer>, Supplier<Integer> {
  private int highestId;

  public static IntIdGenerator fromZeroInclusive() {
    return new IntIdGenerator(0);
  }

  public static IntIdGenerator fromOneInclusive() {
    return new IntIdGenerator(1);
  }

  public static IntIdGenerator fromInclusive(int initial) {
    return new IntIdGenerator(initial);
  }

  protected IntIdGenerator(int initial) {
    this.highestId = initial;
  }

  @Override
  public boolean hasNext() {
    return highestId != Integer.MAX_VALUE;
  }

  public Integer next() {
    if (highestId == Integer.MAX_VALUE) {
      throw new RuntimeException("Ran out of IDs (integer overflow)");
    }
    return increment();
  }

  @Override
  public Integer get() {
    return increment();
  }

  private synchronized Integer increment() {
    return ++highestId;
  }
}
