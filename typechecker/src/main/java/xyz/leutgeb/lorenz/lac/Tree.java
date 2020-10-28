package xyz.leutgeb.lorenz.lac;

import java.util.Objects;
import lombok.Value;

@Value
public class Tree<T> {
  private static final Tree<?> LEAF = new Tree<>();

  public Tree<T> left;
  public T value;
  public Tree<T> right;

  private Tree(Tree<T> left, T value, Tree<T> right) {
    Objects.requireNonNull(left);
    Objects.requireNonNull(value);
    Objects.requireNonNull(right);
    this.left = left;
    this.value = value;
    this.right = right;
  }

  private Tree() {
    this.left = null;
    this.value = null;
    this.right = null;
  }

  public static <T> Tree<T> leaf() {
    return (Tree<T>) LEAF;
  }

  public static <T> Tree<T> node(Tree<T> left, T value, Tree<T> right) {
    return new Tree<>(left, value, right);
  }

  public boolean isLeaf() {
    return this == LEAF;
  }

  @Override
  public String toString() {
    if (LEAF == this) {
      return "leaf";
    }
    return "(" + left + ", " + value + ", " + right + ")";
  }
}
