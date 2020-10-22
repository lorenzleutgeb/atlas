package xyz.leutgeb.lorenz.lac.typing.resources;

import static java.util.Collections.emptyList;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.Token;
import xyz.leutgeb.lorenz.lac.antlr.TacticBaseVisitor;
import xyz.leutgeb.lorenz.lac.antlr.TacticParser;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.WVar;
import xyz.leutgeb.lorenz.lac.util.Fraction;

@Value
@Slf4j
public class TacticVisitorImpl extends TacticBaseVisitor<Object> {
  Prover prover;
  Obligation obligation;

  private static final boolean FIXING_ENABLED = false;
  private static final boolean RECORDING_ENABLED = false;

  private void proveInternal(
      Obligation obligation, TacticParser.TacticExpressionContext tacticExpression) {
    if (tacticExpression instanceof TacticParser.NamedTacticExpressionContext) {
      final var annotatedTacticExpression =
          (TacticParser.NamedTacticExpressionContext) tacticExpression;

      if (RECORDING_ENABLED) {
        final String annotation = annotatedTacticExpression.name.getText();
        if (annotation != null) {
          prover.record(annotation, obligation);
        }
      }

      tacticExpression = annotatedTacticExpression.tacticExpression();
    }

    Token start = null;

    if (tacticExpression instanceof TacticParser.FixedAnnotationContext) {
      final var fixedAnnotationContext = (TacticParser.FixedAnnotationContext) tacticExpression;
      start = fixedAnnotationContext.getStart();
      if (FIXING_ENABLED) {
        final Optional<Annotation> optionalFromFixing =
            convert(obligation.getContext().getAnnotation().size(), fixedAnnotationContext.from);
        final Optional<Annotation> optionalToFixing =
            convert(obligation.getAnnotation().size(), fixedAnnotationContext.to);

        if (optionalFromFixing.isPresent()) {
          log.info(
              "Fixing annotation named '{}' on line {}",
              obligation.getContext().getAnnotation().getNameAndId(),
              fixedAnnotationContext.getStart().getLine());
          log.info(
              "Fixing annotation named '{}' on line {}: {} = {}",
              obligation.getContext().getAnnotation().getNameAndId(),
              fixedAnnotationContext.getStart().getLine(),
              obligation.getContext().getAnnotation(),
              optionalFromFixing.get());

          prover.addExternalConstraints(
              EqualityConstraint.eq(
                  optionalFromFixing.get(),
                  obligation.getContext().reorderLexicographically().getAnnotation(),
                  "(tactic) fixed at position "
                      + start.getLine()
                      + ":"
                      + start.getCharPositionInLine()
                      + " (indices mean lexicographically reordered context)"));
        }

        if (optionalToFixing.isPresent()) {
          log.info(
              "Fixing annotation named '{}' on line {}",
              obligation.getAnnotation().getNameAndId(),
              fixedAnnotationContext.getStart().getLine());

          log.info(
              "Fixing annotation named '{}' on line {}: {} = {}",
              obligation.getAnnotation().getNameAndId(),
              fixedAnnotationContext.getStart().getLine(),
              obligation.getAnnotation(),
              optionalToFixing.get());

          prover.addExternalConstraints(
              EqualityConstraint.eq(
                  optionalToFixing.get(),
                  obligation.getAnnotation(),
                  "(tactic) fixed at position "
                      + start.getLine()
                      + ":"
                      + start.getCharPositionInLine()));
        }
      }

      proveInternal(obligation, fixedAnnotationContext.next);
      return;
    }

    String ruleNameText;
    List<TacticParser.TacticExpressionContext> next;
    if (tacticExpression instanceof TacticParser.TerminalTacticExpressionContext) {
      final var terminal = (TacticParser.TerminalTacticExpressionContext) tacticExpression;
      ruleNameText = terminal.identifier.getText();
      start = terminal.getStart();
      next = emptyList();
    } else if (tacticExpression instanceof TacticParser.ListTacticExpressionContext) {
      final var listTacticExpressionContext =
          (TacticParser.ListTacticExpressionContext) tacticExpression;
      start = listTacticExpressionContext.getStart();
      ruleNameText = listTacticExpressionContext.elements.get(0).getText();
      next =
          listTacticExpressionContext.elements.subList(
              1, listTacticExpressionContext.elements.size());
    } else {
      throw new UnsupportedOperationException();
    }
    if (ruleNameText.equals("?")) {
      log.warn(
          "Leaving hole at position {}:{} for obligation {}",
          start.getLine(),
          start.getCharPositionInLine(),
          obligation);
      return;
    } else if (ruleNameText.startsWith("w:var:")) {
      prover.setWeakenVariables(Boolean.parseBoolean(ruleNameText.substring(6)));

      if (next.size() != 1) {
        throw bug("?");
      }
      proveInternal(obligation, next.get(0));
      return;
    }

    long count = 0;
    List<Obligation> result;
    if (ruleNameText.startsWith("_")) {
      if (!ruleNameText.contains("auto")) {
        log.info(
            "Expanding {} at position {}:{}",
            ruleNameText,
            start.getLine(),
            start.getCharPositionInLine());
        prover.setLogApplications(true);
      }
      prover.setWeakenAggressively(ruleNameText.contains("w"));
      prover.prove(obligation);
      prover.setWeakenAggressively(!ruleNameText.contains("w"));
      if (!ruleNameText.contains("auto")) {
        prover.setLogApplications(false);
      }
      result = emptyList();
    } else {
      if (obligation.getExpression().isTerminal()
          && WVar.redundantId(obligation).isPresent()
          && Set.of("leaf", "node", "var", "app").contains(ruleNameText)) {
        log.info("Automatically removing leftover variables.");
        prover.setLogApplications(true);
        obligation = prover.weakenVariables(obligation);
      }
      result = prover.applyByName(ruleNameText, obligation);
      count = result.stream().filter(x -> x.getCost() != 0).count();
    }

    if (count != next.size()) {
      log.warn(
          "Given tactic does not apply: Rule ("
              + ruleNameText
              + ") applied to \n\n\t\t"
              + obligation
              + "\n\n yields "
              + count
              + " new obligations but "
              + next.size()
              + " are covered.");
    }

    for (int i = 0; i < result.size(); i++) {
      if ((i > 1 && result.get(i).getCost() == 0 && obligation.getCost() == 0)
          || (obligation.getCost() == 1 && result.get(i).getCost() == 0)) {
        prover.prove(result.get(i));
      } else {
        proveInternal(result.get(i), next.get(i));
      }
    }
  }

  @Override
  public Object visitTactic(TacticParser.TacticContext ctx) {
    proveInternal(obligation, ctx.tacticExpression());
    return null;
  }

  public static Optional<Annotation> convert(
      int size, TacticParser.AnnotationContext annotationContext) {
    // if (annotationContext instanceof TacticParser.DontCareAnnotationContext) {
    //	return Optional.empty();
    // }
    if (annotationContext instanceof TacticParser.ZeroAnnotationContext) {
      final var start = annotationContext.getStart();
      return Optional.of(
          Annotation.zero(
              size, "fixed at position " + start.getLine() + ":" + start.getCharPositionInLine()));
    }
    if (annotationContext instanceof TacticParser.NonEmptyAnnotationContext) {
      final var context = (TacticParser.NonEmptyAnnotationContext) annotationContext;
      List<Coefficient> rankCoefficients = new ArrayList<>(size);
      for (int i = 0; i < size; i++) {
        rankCoefficients.add(KnownCoefficient.ZERO);
      }
      Map<List<Integer>, Coefficient> coeffiecients = new HashMap<>();
      for (var entry : context.entries) {
        final var index = entry.index();
        final var value = convert(entry.coefficient);
        if (index instanceof TacticParser.RankIndexContext) {
          final var rankIndex = (TacticParser.RankIndexContext) index;
          rankCoefficients.set(Integer.parseInt(rankIndex.NUMBER().getText()), value);
        } else if (index instanceof TacticParser.OtherIndexContext) {
          final var otherIndex = (TacticParser.OtherIndexContext) index;
          coeffiecients.put(
              otherIndex.elements.stream()
                  .map(Token::getText)
                  .map(Integer::parseInt)
                  .collect(Collectors.toUnmodifiableList()),
              value);
        }
      }
      final var start = context.getStart();
      return Optional.of(
          new Annotation(
              rankCoefficients,
              coeffiecients,
              "fixed at position " + start.getLine() + ":" + start.getCharPositionInLine()));
    }
    throw new IllegalArgumentException("cannot convert context");
  }

  private static KnownCoefficient convert(TacticParser.NumberContext context) {
    if (context instanceof TacticParser.NatContext) {
      return new KnownCoefficient(new Fraction(Integer.parseInt(context.getText())));
    }
    if (context instanceof TacticParser.RatContext) {
      final var ratContext = (TacticParser.RatContext) context;
      return new KnownCoefficient(
          new Fraction(
              Integer.parseInt(ratContext.numerator.getText()),
              Integer.parseInt(ratContext.denominator.getText())));
    }
    throw new IllegalArgumentException("cannot convert context");
  }
}
