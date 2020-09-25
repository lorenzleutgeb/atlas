package xyz.leutgeb.lorenz.lac.typing.resources;

import static java.util.Collections.emptyList;

import java.util.List;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import xyz.leutgeb.lorenz.lac.antlr.TacticBaseVisitor;
import xyz.leutgeb.lorenz.lac.antlr.TacticParser;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;

@Value
@Slf4j
public class TacticVisitorImpl extends TacticBaseVisitor<Object> {
  Prover prover;
  Obligation obligation;

  private void proveInternal(
      Obligation obligation, TacticParser.TacticExpressionContext tacticExpression) {
    if (tacticExpression
        instanceof TacticParser.AnnotatedTacticExpressionContext annotatedTacticExpression) {
      final String annotation = annotatedTacticExpression.annotation.getText();
      tacticExpression = annotatedTacticExpression.tacticExpression();
      if (annotation != null) {
        prover.record(annotation, obligation);
      }
    }

    String ruleNameText;
    List<TacticParser.TacticExpressionContext> next;
    if (tacticExpression instanceof TacticParser.TerminalTacticExpressionContext) {
      ruleNameText =
          ((TacticParser.TerminalTacticExpressionContext) tacticExpression).identifier.getText();
      next = emptyList();
    } else if (tacticExpression
        instanceof TacticParser.ListTacticExpressionContext listTacticExpressionContext) {
      ruleNameText = listTacticExpressionContext.elements.get(0).getText();
      next =
          listTacticExpressionContext.elements.subList(
              1, listTacticExpressionContext.elements.size());
    } else {
      throw new UnsupportedOperationException();
    }
    if (ruleNameText.equals("?")) {
      log.warn("Leaving hole in tactic: {}", obligation);
      return;
    }

    Obligation weakened = prover.weakenVariables(obligation);

    List<Obligation> result;
    if (ruleNameText.startsWith("_")) {
      prover.setAuto(ruleNameText.contains("auto"));
      prover.setTreeCf(ruleNameText.contains("cf"));
      prover.setWeakenBeforeTerminal(ruleNameText.contains("w"));
      prover.prove(obligation);
      result = emptyList();
    } else {
      result = prover.applyByName(ruleNameText, weakened);
    }

    if (result.size() != next.size()) {
      throw new RuntimeException(
          "Given tactic does not apply: Rule ("
              + ruleNameText
              + ") applied to \n\n\t\t"
              + obligation
              + "\n\n yields "
              + result.size()
              + " new obligations but only "
              + next.size()
              + " are covered.");
    }
    for (int i = 0; i < result.size(); i++) {
      proveInternal(result.get(i), next.get(i));
    }
  }

  @Override
  public Object visitTactic(TacticParser.TacticContext ctx) {
    proveInternal(obligation, ctx.tacticExpression());
    return null;
  }
}
