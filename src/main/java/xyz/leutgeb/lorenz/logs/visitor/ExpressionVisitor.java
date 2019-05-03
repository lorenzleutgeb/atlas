package xyz.leutgeb.lorenz.logs.visitor;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import xyz.leutgeb.lorenz.logs.antlr.SplayParser;
import xyz.leutgeb.lorenz.logs.ast.BooleanExpression;
import xyz.leutgeb.lorenz.logs.ast.CallExpression;
import xyz.leutgeb.lorenz.logs.ast.Case;
import xyz.leutgeb.lorenz.logs.ast.ComparisonOperator;
import xyz.leutgeb.lorenz.logs.ast.Expression;
import xyz.leutgeb.lorenz.logs.ast.Identifier;
import xyz.leutgeb.lorenz.logs.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.logs.ast.LetExpression;
import xyz.leutgeb.lorenz.logs.ast.MatchExpression;
import xyz.leutgeb.lorenz.logs.ast.Tuple;
import xyz.leutgeb.lorenz.logs.ast.TupleElement;

public class ExpressionVisitor extends SourceNameAwareVisitor<Expression> {
  public ExpressionVisitor(String sourceName) {
    super(sourceName);
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
    List<Case> cases = new ArrayList<>(ctx.cases.size());
    for (SplayParser.MatchCaseContext matchCase : ctx.cases) {
      SplayParser.PatternContext patternContext = matchCase.pattern();
      SplayParser.ExpressionContext subExpressionContext = matchCase.expression();
      var subExpressionVisitor = new ExpressionVisitor(getSourceName());
      var subExpression = subExpressionVisitor.visit(subExpressionContext);
      if (patternContext.patternTuple() != null && patternContext.IDENTIFIER() == null) {
        cases.add(
            new Case(getSource(matchCase), visit(patternContext.patternTuple()), subExpression));
      } else if (patternContext.patternTuple() == null && patternContext.IDENTIFIER() != null) {
        cases.add(
            new Case(
                getSource(matchCase),
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
    return new CallExpression(
        getSource(ctx),
        Identifier.get(ctx.name.getText()),
        ctx.params.stream().map(this::visit).collect(toList()));
  }

  @Override
  public Expression visitLetExpression(SplayParser.LetExpressionContext ctx) {
    return new LetExpression(
        getSource(ctx), Identifier.get(ctx.name.getText()), visit(ctx.value), visit(ctx.body));
  }

  @Override
  public Expression visitIdentifier(SplayParser.IdentifierContext ctx) {
    return Identifier.get(ctx.getText());
  }

  @Override
  public TupleElement visitTupleElement(SplayParser.TupleElementContext ctx) {
    if (ctx.IDENTIFIER() != null) {
      String variable = ctx.IDENTIFIER().getText();
      return Identifier.get(variable);
    }
    if (ctx.tuple() != null) {
      return visitTuple(ctx.tuple());
    }
    throw new IllegalStateException();
  }

  @Override
  public Tuple visitTuple(SplayParser.TupleContext ctx) {
    return new Tuple(
        getSource(ctx),
        ctx.elements.stream().map(this::visitTupleElement).collect(Collectors.toList()));
  }

  @Override
  public Tuple visitPatternTuple(SplayParser.PatternTupleContext ctx) {
    return new Tuple(
        getSource(ctx),
        Arrays.asList(
            Identifier.get(ctx.left.getText(), getSource(ctx)),
            Identifier.get(ctx.middle.getText(), getSource(ctx)),
            Identifier.get(ctx.right.getText(), getSource(ctx))));
  }

  @Override
  public Expression visitParenthesizedExpression(SplayParser.ParenthesizedExpressionContext ctx) {
    return visit(ctx.expression());
  }
}
