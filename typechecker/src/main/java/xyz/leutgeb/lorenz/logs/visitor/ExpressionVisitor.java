package xyz.leutgeb.lorenz.logs.visitor;

import static java.util.stream.Collectors.toList;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.BooleanExpression;
import xyz.leutgeb.lorenz.logs.ast.CallExpression;
import xyz.leutgeb.lorenz.logs.ast.ComparisonOperator;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.ast.Identifier;
import xyz.leutgeb.lorenz.logs.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.logs.ast.LetExpression;
import xyz.leutgeb.lorenz.logs.ast.MatchExpression;
import xyz.leutgeb.lorenz.logs.ast.Number;
import xyz.leutgeb.lorenz.logs.ast.Tuple;

public class ExpressionVisitor extends SourceNameAwareVisitor<Expression> {
  public ExpressionVisitor(String moduleName, Path path) {
    super(moduleName, path);
  }

  @Override
  public Expression visitIteExpression(SplayParser.IteExpressionContext ctx) {
    return new IfThenElseExpression(
        getSource(ctx), visit(ctx.condition()), visit(ctx.truthy), visit(ctx.falsy));
  }

  @Override
  public Expression visitComparison(SplayParser.ComparisonContext ctx) {
    var l = ExpressionVisitor.this.visit(ctx.left);
    var r = ExpressionVisitor.this.visit(ctx.right);
    return new BooleanExpression(
        getSource(ctx), l, ComparisonOperator.fromToken(ctx.op().getText()), r);
  }

  @Override
  public Expression visitMatchExpression(SplayParser.MatchExpressionContext ctx) {
    List<Pair<Expression, Expression>> cases = new ArrayList<>(ctx.cases.size());
    for (SplayParser.MatchCaseContext matchCase : ctx.cases) {
      SplayParser.PatternContext patternContext = matchCase.pattern();
      SplayParser.ExpressionContext subExpressionContext = matchCase.expression();
      var subExpressionVisitor = new ExpressionVisitor(getModuleName(), getPath());
      var subExpression = subExpressionVisitor.visit(subExpressionContext);
      if (patternContext.patternTuple() != null && patternContext.IDENTIFIER() == null) {
        cases.add(new Pair<>(visit(patternContext.patternTuple()), subExpression));
      } else if (patternContext.patternTuple() == null && patternContext.IDENTIFIER() != null) {
        cases.add(
            new Pair<>(
                Identifier.get(patternContext.IDENTIFIER().getText(), getSource(patternContext)),
                subExpression));
      } else {
        throw new IllegalArgumentException();
      }
    }

    return new MatchExpression(getSource(ctx), visit(ctx.expression()), cases);
  }

  @Override
  public Expression visitTupleExpression(SplayParser.TupleExpressionContext ctx) {
    return super.visitTupleExpression(ctx);
  }

  @Override
  public Expression visitCallExpression(SplayParser.CallExpressionContext ctx) {
    var fqn = ctx.name.getText();
    if (!fqn.contains(".")) {
      fqn = getModuleName() + "." + fqn;
    }
    return new CallExpression(
        getSource(ctx),
        getModuleName(),
        Identifier.get(fqn, getSource(ctx)),
        ctx.params.stream().map(this::visit).collect(toList()));
  }

  @Override
  public Expression visitLetExpression(SplayParser.LetExpressionContext ctx) {
    return new LetExpression(
        getSource(ctx),
        Identifier.get(ctx.name.getText(), getSource(ctx)),
        visit(ctx.value),
        visit(ctx.body));
  }

  @Override
  public Expression visitIdentifier(SplayParser.IdentifierContext ctx) {
    return Identifier.get(ctx.getText(), getSource(ctx));
  }

  @Override
  public Tuple visitTuple(SplayParser.TupleContext ctx) {
    var elements = new ArrayList<Expression>(3);
    elements.add(visit(ctx.left));
    elements.add(visit(ctx.middle));
    elements.add(visit(ctx.right));
    return new Tuple(getSource(ctx), elements);
  }

  @Override
  public Tuple visitPatternTuple(SplayParser.PatternTupleContext ctx) {
    return new Tuple(
        getSource(ctx),
        List.of(
            Identifier.get(ctx.left.getText(), getSource(ctx)),
            Identifier.get(ctx.middle.getText(), getSource(ctx)),
            Identifier.get(ctx.right.getText(), getSource(ctx))));
  }

  @Override
  public Expression visitParenthesizedExpression(SplayParser.ParenthesizedExpressionContext ctx) {
    return visit(ctx.expression());
  }

  @Override
  public Expression visitConstant(SplayParser.ConstantContext ctx) {
    return new Number(getSource(ctx), Integer.valueOf(ctx.NUMBER().getText()));
  }
}
