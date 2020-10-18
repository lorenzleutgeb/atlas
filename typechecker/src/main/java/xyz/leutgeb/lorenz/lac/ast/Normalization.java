package xyz.leutgeb.lorenz.lac.ast;

import lombok.AllArgsConstructor;
import lombok.Value;

@Value
@AllArgsConstructor
public class Normalization {
  public Identifier identifier;
  public Expression expression;
}
