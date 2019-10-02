package xyz.leutgeb.lorenz.logs.ast.sources;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class Derived extends Source {
  public static Derived desugar(Source source) {
    return new Derived(source, Justification.DESUGAR);
  }

  public enum Justification {
    ANF,
    DESUGAR
  }

  Source parent;
  Justification justification;

  public static Derived anf(Source parent) {
    return new Derived(parent, Justification.ANF);
  }

  @Override
  public Source getRoot() {
    return parent.getRoot();
  }

  @Override
  public String toString() {
    return getRoot().toString() + "(" + getJustification() + ")";
  }
}
