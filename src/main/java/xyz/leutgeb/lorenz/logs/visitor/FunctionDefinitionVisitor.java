package xyz.leutgeb.lorenz.logs.visitor;

import java.util.ArrayList;
import java.util.List;
import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.FunctionDefinition;

public class FunctionDefinitionVisitor extends SourceNameAwareVisitor<FunctionDefinition> {
  public FunctionDefinitionVisitor(String sourceName) {
    super(sourceName);
  }

  @Override
  public FunctionDefinition visitFunc(SplayParser.FuncContext ctx) {
    List<String> args = new ArrayList<>(ctx.args.size());
    for (Token t : ctx.args) {
      String arg = t.getText();
      args.add(arg);
    }
    ExpressionVisitor visitor = new ExpressionVisitor(getSourceName());
    return new FunctionDefinition(ctx.name.getText(), args, visitor.visit(ctx.body));
  }
}
