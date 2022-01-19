package xyz.leutgeb.lorenz.atlas.ast.visitors;

import static java.util.stream.Collectors.toList;

import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.antlr.SplayParser;
import xyz.leutgeb.lorenz.atlas.ast.*;
import xyz.leutgeb.lorenz.atlas.ast.expressions.*;
import xyz.leutgeb.lorenz.atlas.util.IntIdGenerator;

class ExpressionVisitor extends SourceNameAwareVisitor<Expression> {
  private final IntIdGenerator idGenerator = IntIdGenerator.fromInclusive(Integer.MAX_VALUE / 2);

  public ExpressionVisitor(String moduleName, Path path) {
    super(moduleName, path);
  }

  @Override
  public Expression visitTickExpression(SplayParser.TickExpressionContext ctx) {
    final var numerator = ctx.numerator == null ? 1 : Integer.parseInt(ctx.numerator.getText());
    final var denominator =
        ctx.denominator == null ? 1 : Integer.parseInt(ctx.denominator.getText());
    final var cost = new Fraction(numerator, denominator);
    return new TickExpression(getSource(ctx), visit(ctx.expression()), cost);
  }

  @Override
  public Expression visitCoinExpression(SplayParser.CoinExpressionContext ctx) {
    final var numerator = ctx.numerator == null ? 1 : Integer.parseInt(ctx.numerator.getText());
    final var denominator =
        ctx.denominator == null ? 2 : Integer.parseInt(ctx.denominator.getText());
    final var cost = new Fraction(numerator, denominator);
    return new CoinExpression(getSource(ctx), cost);
  }

  @Override
  public Expression visitHoleExpression(SplayParser.HoleExpressionContext ctx) {
    return new HoleExpression(getSource(ctx));
  }

  @Override
  public Expression visitIteExpression(SplayParser.IteExpressionContext ctx) {
    return new IfThenElseExpression(
        getSource(ctx), visit(ctx.condition), visit(ctx.truthy), visit(ctx.falsy));
  }

  @Override
  public Expression visitMatchTupleExpression(SplayParser.MatchTupleExpressionContext ctx) {
    return new MatchTupleExpression(
        getSource(ctx),
        visit(ctx.test),
        visit(ctx.body),
        (TupleExpression) visit(ctx.tuplePattern()));
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
    return new MatchTreeExpression(
        getSource(ctx),
        visit(ctx.test),
        ctx.leafCase != null ? visit(ctx.leafCase) : IdentifierExpression.leaf(),
        visit(ctx.nodePattern),
        visit(ctx.nodeCase));
  }

  @Override
  public Expression visitCallExpression(SplayParser.CallExpressionContext ctx) {
    var fqn = ctx.name.getText();
    final int lastDot = fqn.lastIndexOf(".");
    final var moduleName = lastDot > -1 ? fqn.substring(0, lastDot) : getModuleName();
    final var name = lastDot > -1 ? fqn.substring(lastDot + 1) : fqn;
    return new CallExpression(
        getSource(ctx),
        moduleName,
        IdentifierExpression.get(name, getSource(ctx)),
        ctx.params.stream().map(this::visit).collect(toList()));
  }

  @Override
  public Expression visitLetExpression(SplayParser.LetExpressionContext ctx) {
    return new LetExpression(
        getSource(ctx),
        IdentifierExpression.get(ctx.name.getText(), getSource(ctx)),
        visit(ctx.value),
        visit(ctx.body));
  }

  @Override
  public Expression visitIdentifier(SplayParser.IdentifierContext ctx) {
    return IdentifierExpression.get(ctx.getText(), getSource(ctx));
  }

  @Override
  public TupleExpression visitTuple(SplayParser.TupleContext ctx) {
    var elements = new ArrayList<Expression>(ctx.expression().size());
    for (int i = 0; i < ctx.expression().size(); i++) {
      elements.add(visit(ctx.expression(i)));
    }
    return new TupleExpression(getSource(ctx), elements);
  }

  @Override
  public Expression visitNodeExpression(SplayParser.NodeExpressionContext ctx) {
    var elements = new ArrayList<Expression>(3);
    elements.add(visit(ctx.left));
    elements.add(visit(ctx.middle));
    elements.add(visit(ctx.right));
    return new NodeExpression(getSource(ctx), elements);
  }

  @Override
  public Expression visitDeconstructionPattern(SplayParser.DeconstructionPatternContext ctx) {
    return new NodeExpression(
        getSource(ctx), List.of(visit(ctx.left), visit(ctx.middle), visit(ctx.right)));
  }

  @Override
  public Expression visitMaybeAnonymousIdentifier(SplayParser.MaybeAnonymousIdentifierContext ctx) {
    if (ctx.UNDERSCORE() != null) {
      return IdentifierExpression.anonymous(getSource(ctx), idGenerator);
    }
    return IdentifierExpression.get(ctx.IDENTIFIER().getText(), getSource(ctx));
  }

  @Override
  public Expression visitAliasingPattern(SplayParser.AliasingPatternContext ctx) {
    return visitMaybeAnonymousIdentifier(ctx.maybeAnonymousIdentifier());
  }

  @Override
  public Expression visitTuplePattern(SplayParser.TuplePatternContext ctx) {
    var elements = new ArrayList<Expression>(ctx.identifier().size());
    for (int i = 0; i < ctx.identifier().size(); i++) {
      elements.add(visit(ctx.identifier(i)));
    }
    return new TupleExpression(getSource(ctx), elements);
  }

  @Override
  public Expression visitParenthesizedExpression(SplayParser.ParenthesizedExpressionContext ctx) {
    return visit(ctx.expression());
  }

  @Override
  public Expression visitConstant(SplayParser.ConstantContext ctx) {
    throw new UnsupportedOperationException("literal numbers are not implemented");
  }
}
