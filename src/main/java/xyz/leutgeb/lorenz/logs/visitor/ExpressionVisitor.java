package xyz.leutgeb.lorenz.logs.visitor;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.TerminalNode;
import xyz.leutgeb.lorenz.logs.SymbolTable;
import xyz.leutgeb.lorenz.logs.antlr.SplayBaseVisitor;
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

public class ExpressionVisitor extends SplayBaseVisitor<Expression> {
  private final SymbolTable symbolTable;
  private final BooleanExpressionVisitor booleanExpressionVisitor = new BooleanExpressionVisitor();

  public ExpressionVisitor(SymbolTable symbolTable) {
    this.symbolTable = symbolTable;
  }

  private final class BooleanExpressionVisitor extends SplayBaseVisitor<BooleanExpression> {
    @Override
    public BooleanExpression visitCondition(SplayParser.ConditionContext ctx) {
      Expression l = ExpressionVisitor.this.visit(ctx.left);
      Expression r = ExpressionVisitor.this.visit(ctx.right);
      return new BooleanExpression(l, ComparisonOperator.fromToken(ctx.op().getText()), r);
    }
  }

  @Override
  public Expression visitIteExpression(SplayParser.IteExpressionContext ctx) {
    Expression truthy = visit(ctx.truthy);
    Expression falsy = visit(ctx.falsy);
    return new IfThenElseExpression(
        booleanExpressionVisitor.visitCondition(ctx.condition()), truthy, falsy);
  }

  private void ingestPattern(SplayParser.PatternContext patternContext, SymbolTable symbolTable) {
    ingestTuple(patternContext.tuple(), symbolTable);
    ingestIdentifier(patternContext.IDENTIFIER(), patternContext.start, symbolTable);
  }

  private void ingestTuple(SplayParser.TupleContext variableTupleContext, SymbolTable symbolTable) {
    if (variableTupleContext == null) {
      return;
    }
    for (SplayParser.TupleElementContext elementContext : variableTupleContext.elements) {
      ingestTuple(elementContext.tuple(), symbolTable);
      ingestIdentifier(elementContext.IDENTIFIER(), elementContext.start, symbolTable);
    }
  }

  private void ingestIdentifier(TerminalNode variableNode, Token start, SymbolTable symbolTable) {
    if (variableNode == null) {
      return;
    }
    String variable = variableNode.getText();
    symbolTable.put(Identifier.get(variable), new SymbolTable.Entry(null, start));
  }

  @Override
  public Expression visitMatchExpression(SplayParser.MatchExpressionContext ctx) {
    List<Case> cases = new ArrayList<>(ctx.cases.size());
    for (SplayParser.MatchCaseContext matchCase : ctx.cases) {
      SplayParser.PatternContext patternContext = matchCase.pattern();
      SplayParser.ExpressionContext subExpressionContext = matchCase.expression();
      SymbolTable symbolTable = new SymbolTable(this.symbolTable);
      ingestPattern(patternContext, symbolTable);
      var subExpressionVisitor = new ExpressionVisitor(symbolTable);
      var subExpression = subExpressionVisitor.visit(subExpressionContext);
      if (patternContext.tuple() != null && patternContext.IDENTIFIER() == null) {
        cases.add(new Case(visit(patternContext.tuple()), subExpression));
      } else if (patternContext.tuple() == null && patternContext.IDENTIFIER() != null) {
        cases.add(new Case(Identifier.get(patternContext.IDENTIFIER().getText()), subExpression));
      } else {
        throw new IllegalArgumentException();
      }
    }
    return new MatchExpression(visit(ctx.expression()), cases);
  }

  @Override
  public Expression visitTupleExpression(SplayParser.TupleExpressionContext ctx) {
    return super.visitTupleExpression(ctx);
  }

  @Override
  public Expression visitCallExpression(SplayParser.CallExpressionContext ctx) {
    return new CallExpression(
        Identifier.get(ctx.name.getText()), ctx.params.stream().map(this::visit).collect(toList()));
  }

  @Override
  public Expression visitLetExpression(SplayParser.LetExpressionContext ctx) {
    SymbolTable symbolTable = new SymbolTable(this.symbolTable);
    ingestIdentifier(ctx.IDENTIFIER(), ctx.name, symbolTable);
    var subExpressionVisitor = new ExpressionVisitor(symbolTable);
    return new LetExpression(
        Identifier.get(ctx.name.getText()), subExpressionVisitor.visit(ctx.body));
  }

  @Override
  public Expression visitMatchCase(SplayParser.MatchCaseContext ctx) {
    SymbolTable symbolTable = new SymbolTable(this.symbolTable);
    ingestPattern(ctx.pattern(), symbolTable);
    var subExpressionVisitor = new ExpressionVisitor(symbolTable);
    return new Case(
        visitTuple(ctx.pattern().tuple()), subExpressionVisitor.visit(ctx.expression()));
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
        ctx.elements.stream().map(this::visitTupleElement).collect(Collectors.toList()));
  }
}
