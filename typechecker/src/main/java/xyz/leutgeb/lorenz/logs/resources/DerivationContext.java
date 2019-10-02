package xyz.leutgeb.lorenz.logs.resources;

import java.util.List;
import org.antlr.v4.tool.Rule;
import xyz.leutgeb.lorenz.logs.ast.Expression;

public class DerivationContext {
  Expression current;
  List<Rule> applications;
}
