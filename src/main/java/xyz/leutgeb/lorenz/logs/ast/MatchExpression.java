package xyz.leutgeb.lorenz.logs.ast;

import java.util.List;
import lombok.Data;

@Data
public class MatchExpression extends Expression {
  private final Expression test;
  private final List<Case> cases;
}
