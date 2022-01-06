package xyz.leutgeb.lorenz.atlas.typing.resources;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static java.util.Optional.ofNullable;
import static java.util.stream.Collectors.toUnmodifiableMap;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

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
import org.hipparchus.fraction.Fraction;
import xyz.leutgeb.lorenz.atlas.antlr.TacticBaseVisitor;
import xyz.leutgeb.lorenz.atlas.antlr.TacticParser;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.EqualityConstraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Prover;
import xyz.leutgeb.lorenz.atlas.typing.resources.rules.WVar;

@Value
@Slf4j
public class TacticVisitorImpl extends TacticBaseVisitor<Object> {
  Prover prover;
  Obligation obligation;

  private static final boolean FIXING_ENABLED = false;
  private static final boolean RECORDING_ENABLED = true;

  private void proveInternal(Obligation obligation, TacticParser.ListContext list) {
    final var initial = list.first;

    List<Obligation> remaining = proveInternal(obligation, initial);

    if (initial instanceof TacticParser.RuleContext) {
      final var rule = (TacticParser.RuleContext) initial;
      if (rule.ruleName.getText().equals("let:tree:cf")) {
        final ArrayList<Obligation> remains = new ArrayList<>(remaining.size());
        for (int i = 0; i < remaining.size(); i++) {
          if (!remaining.get(i).isCost() && i > 1) {
            proveInternalAny(remaining.get(i), list.elements.get(0));
            continue;
          }
          remains.add(remaining.get(i));
        }
        remaining = remains;
      }
    }

    if (remaining.size() != list.elements.size()) {
      throw bug(
          "Applying "
              + initial.getText()
              + " to "
              + obligation
              + " yields "
              + remaining.size()
              + " new obligations, but "
              + list.elements.size()
              + " are given.");
    }

    for (int i = 0; i < remaining.size(); i++) {
      proveInternalAny(remaining.get(i), list.elements.get(i));
    }
  }

  private void proveInternalAny(
      Obligation obligation, TacticParser.TacticExpressionOrListContext any) {
    if (any instanceof TacticParser.ListContext) {
      proveInternal(obligation, (TacticParser.ListContext) any);
      return;
    }

    List<Obligation> remaining =
        proveInternal(obligation, ((TacticParser.ImmediateContext) any).tacticExpression());
    if (!remaining.isEmpty()) {
      throw bug("does not consume all");
    }
  }

  private List<Obligation> proveInternal(
      Obligation obligation, TacticParser.TacticExpressionContext immediateContext) {
    if (immediateContext instanceof final TacticParser.FixContext fixedAnnotationContext) {
      fix(obligation, fixedAnnotationContext);
      if (RECORDING_ENABLED && fixedAnnotationContext.applicationName != null) {
        prover.record(fixedAnnotationContext.applicationName.getText(), obligation);
      }
      return List.of(obligation);
    } else if (!(immediateContext instanceof TacticParser.RuleContext)) {
      throw new RuntimeException();
    }

    final var annotatedTacticExpression = (TacticParser.RuleContext) immediateContext;

    final var ruleName = annotatedTacticExpression.ruleName.getText();
    final var start = annotatedTacticExpression.getStart();
    final var arguments = convert(annotatedTacticExpression.arguments);

    if (RECORDING_ENABLED
        && annotatedTacticExpression.applicationName != null
        && obligation.isCost()) {
      prover.record(annotatedTacticExpression.applicationName.getText(), obligation);
    }

    if (ruleName.equals("?")) {
      log.warn(
          "Leaving hole at position {}:{} for obligation {}",
          start.getLine(),
          start.getCharPositionInLine(),
          obligation);
      return emptyList();
    } else if ("w:var".equals(ruleName)) {
      prover.setWeakenVariables(Boolean.parseBoolean(arguments.get("set")));
      return singletonList(obligation);
    }

    List<Obligation> result;
    if ("_".equals(ruleName)) {
      final var auto = "true".equals(arguments.get("auto"));
      final var weakenAggressively = "true".equals(arguments.get("w"));
      if (!auto) {
        log.debug(
            "Expanding {} at position {}:{}",
            ruleName,
            start.getLine(),
            start.getCharPositionInLine());
        // prover.setLogApplications(true);
      }
      prover.prove(obligation);
      if (!auto) {
        prover.setLogApplications(false);
      }
      result = emptyList();
    } else {
      final List<Identifier> redundant = WVar.redundantIds(obligation).toList();
      if (obligation.getExpression().isTerminal()
          && !redundant.isEmpty()
          && Set.of("leaf", "node", "var", "app", "tick").contains(ruleName)) {
        log.debug(
            "Automatically applying (w:var) to remove leftover variables {} before applying {} on line {}.",
            redundant,
            ruleName,
            start.getLine());
        // prover.setLogApplications(true);
        obligation = prover.weakenVariables(obligation);
      }
      try {
        result = prover.applyByName(ruleName, arguments, obligation);
      } catch (Exception e) {
        log.error("Error in line {}", start.getLine(), e);
        throw e;
      }
    }

    return result;
  }

  private void fix(Obligation obligation, TacticParser.FixContext fixedAnnotationContext) {
    if (!FIXING_ENABLED) {
      return;
    }

    final var start = fixedAnnotationContext.getStart();
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

  @Override
  public Object visitTactic(TacticParser.TacticContext ctx) {
    proveInternalAny(obligation, ctx.tacticExpressionOrList());
    return null;
  }

  public static Optional<Annotation> convert(
      int size, TacticParser.AnnotationContext annotationContext) {
    if (annotationContext instanceof TacticParser.ZeroAnnotationContext) {
      final var start = annotationContext.getStart();
      return Optional.of(
          Annotation.zero(
              size, "fixed at position " + start.getLine() + ":" + start.getCharPositionInLine()));
    }
    if (annotationContext instanceof final TacticParser.NonEmptyAnnotationContext context) {
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
    if (context instanceof final TacticParser.RatContext ratContext) {
      return new KnownCoefficient(
          new Fraction(
              Integer.parseInt(ratContext.numerator.getText()),
              Integer.parseInt(ratContext.denominator.getText())));
    }
    throw new IllegalArgumentException("cannot convert context");
  }

  private static Map<String, String> convert(List<TacticParser.ArgumentMapEntryContext> entries) {
    return entries.stream()
        .collect(
            toUnmodifiableMap(
                e -> e.key.getText(), e -> ofNullable(e.value).map(Token::getText).orElse("true")));
  }
}
