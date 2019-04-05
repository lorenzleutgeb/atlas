package xyz.leutgeb.lorenz.logs.ast;

import lombok.Value;

@Value
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
}
