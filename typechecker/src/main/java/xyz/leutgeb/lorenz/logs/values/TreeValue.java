package xyz.leutgeb.lorenz.logs.values;

import java.util.Objects;
import lombok.Value;

@Value
public class TreeValue<T> {
  private static final TreeValue<?> NIL = new TreeValue<>();

  private TreeValue<T> left;
  private TreeValue<T> right;
  private T value;

  public static <T> TreeValue<T> nil() {
    @SuppressWarnings("unchecked")
    TreeValue<T> nil = (TreeValue<T>) NIL;
    return nil;
  }

  private TreeValue() {
    left = null;
    right = null;
    value = null;
  }

  public TreeValue(TreeValue<T> left, T value, TreeValue<T> right) {
    Objects.requireNonNull(left);
    Objects.requireNonNull(value);
    Objects.requireNonNull(right);
    this.left = left;
    this.value = value;
    this.right = right;
  }
}
