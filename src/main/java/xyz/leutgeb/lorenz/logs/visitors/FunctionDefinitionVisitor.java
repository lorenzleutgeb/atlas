package xyz.leutgeb.lorenz.logs.visitors;

import java.util.List;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.FunctionDefinition;

public class FunctionDefinitionVisitor extends SplayBaseVisitor<FunctionDefinition> {
  @Override
  public FunctionDefinition visitFunc(SplayParser.FuncContext ctx) {
    final List<String> args = ctx.args.stream().map(Token::toString).collect(Collectors.toList());
    ExpressionVisitor visitor = new ExpressionVisitor(args);
    return new FunctionDefinition(ctx.name.toString(), args, visitor.visit(ctx.body));
  }
}
