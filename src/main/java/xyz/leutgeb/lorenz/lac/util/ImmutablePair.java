package xyz.leutgeb.lorenz.lac.util;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = false)
public class ImmutablePair<K, V> extends Pair<K, V> {
  K left;
  V right;

  @Override
  public V setValue(V value) {
    throw new UnsupportedOperationException();
  }
}
