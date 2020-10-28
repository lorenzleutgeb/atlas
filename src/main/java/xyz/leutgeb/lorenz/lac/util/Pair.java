package xyz.leutgeb.lorenz.lac.util;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * A pair consisting of two elements.
 *
 * <p>This class is an abstract implementation defining the basic API. It refers to the elements as
 * 'left' and 'right'. It also implements the {@code Map.Entry} interface where the key is 'left'
 * and the value is 'right'.
 *
 * <p>Subclass implementations may be mutable or immutable. However, there is no restriction on the
 * type of the stored objects that may be stored. If mutable objects are stored in the pair, then
 * the pair itself effectively becomes mutable.
 *
 * @param <L> the left element type
 * @param <R> the right element type
 * @since 3.0
 */
public abstract class Pair<L, R> implements Map.Entry<L, R>, Serializable {

  /**
   * Obtains an immutable pair of two objects inferring the generic types.
   *
   * <p>This factory allows the pair to be created using inference to obtain the generic types.
   *
   * @param <L> the left element type
   * @param <R> the right element type
   * @param left the left element, may be null
   * @param right the right element, may be null
   * @return a pair formed from the two parameters, not null
   */
  public static <L, R> Pair<L, R> of(final L left, final R right) {
    return new ImmutablePair<>(left, right);
  }

  public static <E> Pair<E, E> of(final List<E> list) {
    if (list.size() != 2) {
      throw new IllegalArgumentException("can only convert list with exactly two elements to pair");
    }
    return Pair.of(list.get(0), list.get(1));
  }

  public static <E> List<Pair<E, E>> pairs(final List<E> list) {
    if (list.size() % 2 != 0) {
      throw new IllegalArgumentException(
          "can only convert list with even number of elements to pairs");
    }
    final List<Pair<E, E>> result = new ArrayList<Pair<E, E>>(list.size() / 2);
    for (int i = 0; i < list.size() - 1; i += 2) {
      result.add(Pair.of(list.get(i), list.get(i + 1)));
    }
    return result;
  }

  // -----------------------------------------------------------------------
  /**
   * Gets the left element from this pair.
   *
   * <p>When treated as a key-value pair, this is the key.
   *
   * @return the left element, may be null
   */
  public abstract L getLeft();

  /**
   * Gets the right element from this pair.
   *
   * <p>When treated as a key-value pair, this is the value.
   *
   * @return the right element, may be null
   */
  public abstract R getRight();

  /**
   * Gets the key from this pair.
   *
   * <p>This method implements the {@code Map.Entry} interface returning the left element as the
   * key.
   *
   * @return the left element as the key, may be null
   */
  @Override
  public final L getKey() {
    return getLeft();
  }

  /**
   * Gets the value from this pair.
   *
   * <p>This method implements the {@code Map.Entry} interface returning the right element as the
   * value.
   *
   * @return the right element as the value, may be null
   */
  @Override
  public R getValue() {
    return getRight();
  }

  // -----------------------------------------------------------------------

  /**
   * Compares this pair to another based on the two elements.
   *
   * @param obj the object to compare to, null returns false
   * @return true if the elements of the pair are equal
   */
  @Override
  public boolean equals(final Object obj) {
    if (obj == this) {
      return true;
    }
    if (obj instanceof Map.Entry<?, ?>) {
      final Map.Entry<?, ?> other = (Map.Entry<?, ?>) obj;
      return Objects.equals(getKey(), other.getKey())
          && Objects.equals(getValue(), other.getValue());
    }
    return false;
  }

  /**
   * Returns a suitable hash code. The hash code follows the definition in {@code Map.Entry}.
   *
   * @return the hash code
   */
  @Override
  public int hashCode() {
    // see Map.Entry API specification
    return (getKey() == null ? 0 : getKey().hashCode())
        ^ (getValue() == null ? 0 : getValue().hashCode());
  }

  /**
   * Returns a String representation of this pair using the format {@code ($left,$right)}.
   *
   * @return a string describing this object, not null
   */
  @Override
  public String toString() {
    return "(" + getLeft() + ',' + getRight() + ')';
  }

  /**
   * Formats the receiver using the given format.
   *
   * <p>This uses {@link java.util.Formattable} to perform the formatting. Two variables may be used
   * to embed the left and right elements. Use {@code %1$s} for the left element (key) and {@code
   * %2$s} for the right element (value). The default format used by {@code toString()} is {@code
   * (%1$s,%2$s)}.
   *
   * @param format the format string, optionally containing {@code %1$s} and {@code %2$s}, not null
   * @return the formatted string, not null
   */
  public String toString(final String format) {
    return String.format(format, getLeft(), getRight());
  }
}
