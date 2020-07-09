package xyz.leutgeb.lorenz.lac;

import java.util.Iterator;

public class IntIdGenerator implements Iterator<Integer> {
  private int highestId;

  public IntIdGenerator() {
    this(0);
  }

  public IntIdGenerator(int initial) {
    this.highestId = initial;
  }

  public static IntIdGenerator human() {
    return new IntIdGenerator(1);
  }

  @Override
  public boolean hasNext() {
    return highestId != Integer.MAX_VALUE;
  }

  public Integer next() {
    if (highestId == Integer.MAX_VALUE) {
      throw new RuntimeException("Ran out of IDs (integer overflow)");
    }
    return highestId++;
  }

  public void reset() {
    highestId = 0;
  }
}
