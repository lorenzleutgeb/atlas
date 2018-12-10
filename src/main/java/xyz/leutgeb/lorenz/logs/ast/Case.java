package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;

@Data
public class Case extends Expression {
  private final Expression matcher;
  private final Expression body;
}
