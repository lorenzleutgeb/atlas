package xyz.leutgeb.lorenz.logs.visitor;

import static java.util.stream.Collectors.toList;

import java.util.ArrayList;
import java.util.List;
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
import xyz.leutgeb.lorenz.logs.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.logs.ast.LetExpression;
import xyz.leutgeb.lorenz.logs.ast.MatchExpression;
import xyz.leutgeb.lorenz.logs.ast.Pattern;
import xyz.leutgeb.lorenz.logs.ast.VariableExpression;

public class ExpressionVisitor extends SplayBaseVisitor<Expression> {
  private final SymbolTable symbolTable;
  private final BooleanExpressionVisitor booleanExpressionVisitor = new BooleanExpressionVisitor();

  public ExpressionVisitor(SymbolTable symbolTable) {
    this.symbolTable = symbolTable;
  }

  private final class BooleanExpressionVisitor extends SplayBaseVisitor<BooleanExpression> {
    @Override
    public BooleanExpression visitCondition(SplayParser.ConditionContext ctx) {
      return new BooleanExpression(
          ExpressionVisitor.this.visit(ctx.left),
          ComparisonOperator.fromToken(ctx.op().getText()),
          ExpressionVisitor.this.visit(ctx.right));
    }
  }

  @Override
  public Expression visitIteExpression(SplayParser.IteExpressionContext ctx) {
    return new IfThenElseExpression(
        booleanExpressionVisitor.visitCondition(ctx.condition()),
        visit(ctx.truthy),
        visit(ctx.falsy));
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
    symbolTable.put(new VariableExpression(variable), new SymbolTable.Entry(null, start));
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
      cases.add(new Case(new Pattern(), subExpression));
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
        ctx.name.getText(), ctx.params.stream().map(this::visit).collect(toList()));
  }

  @Override
  public Expression visitLetExpression(SplayParser.LetExpressionContext ctx) {
    SymbolTable symbolTable = new SymbolTable(this.symbolTable);
    ingestIdentifier(ctx.IDENTIFIER(), ctx.name, symbolTable);
    var subExpressionVisitor = new ExpressionVisitor(symbolTable);
    return new LetExpression(ctx.name.getText(), subExpressionVisitor.visit(ctx.body));
  }
}
