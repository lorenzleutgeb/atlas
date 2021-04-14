package xyz.leutgeb.lorenz.atlas.ast.sources;

import lombok.EqualsAndHashCode;
import lombok.Value;
import xyz.leutgeb.lorenz.atlas.ast.Expression;

@Value
@EqualsAndHashCode(callSuper = true)
public class Derived extends Source {
  Expression parent;
  Justification justification;

  public static Derived desugar(Expression source) {
    return new Derived(source, Justification.DESUGAR);
  }

  public static Derived anf(Expression parent) {
    return new Derived(parent, Justification.ANF);
  }

  public static Derived unshare(Expression parent) {
    return new Derived(parent, Justification.UNSHARE);
  }

  public static Derived rename(Expression parent) {
    return new Derived(parent, Justification.RENAME);
  }

  @Override
  public Source getRoot() {
    return parent.getSource().getRoot();
  }

  @Override
  public String toString() {
    return getRoot().toString() + "(" + getJustification() + ")";
  }

  public enum Justification {
    ANF,
    DESUGAR,
    UNSHARE,
    RENAME,
  }
}
