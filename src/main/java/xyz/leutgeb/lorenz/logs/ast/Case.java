package xyz.leutgeb.lorenz.logs.ast;

import lombok.Data;
import lombok.NonNull;

@Data
public class Case extends Expression {
  @NonNull private final Expression matcher;
  @NonNull private final Expression body;
}
