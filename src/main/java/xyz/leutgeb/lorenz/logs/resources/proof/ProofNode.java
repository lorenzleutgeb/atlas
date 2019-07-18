package xyz.leutgeb.lorenz.logs.resources.proof;

import java.util.Set;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.resources.constraints.Constraint;

public class ProofNode {
  public static enum Rule {
    Let,
    Match,
    Node,
    Nil,
    W,
    WVar,
    App,
    Var,
    Share,
    Ite,
    Cmp
  }

  Expression generator;
  Rule rule;
  Set<Constraint> preconditions;
  Set<ProofNode> children;
}
