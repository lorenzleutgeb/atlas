package xyz.leutgeb.lorenz.atlas.typing.resources.proving;

import static guru.nidi.graphviz.attribute.Label.html;
import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;
import lombok.Value;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.atlas.ast.Expression;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.util.NidiAttribute;
import xyz.leutgeb.lorenz.atlas.util.Util;

// TODO(lorenzleutgeb): Maybe we need to navigate around obligations in a DAG/proof tree?
@Value
public class Obligation {
  AnnotatingContext context;
  Expression expression;
  Annotation annotation;
  boolean cost;
  Optional<Obligation> parent;

  public Obligation(
      AnnotatingContext context,
      Expression expression,
      Annotation annotation,
      boolean cost,
      Optional<Obligation> parent) {
    this.context = context;
    this.expression = expression;

    if (annotation.size() > 1) {
      throw bug(
          "expression being judged cannot be annotated with an annotation that has more than one element");
    }

    this.annotation = annotation;
    this.cost = cost;
    this.parent = parent;
  }

  public Obligation(
      List<Identifier> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation,
      Optional<Obligation> parent) {
    this(contextIds, contextAnnotation, expression, annotation, true, parent);
  }

  public Obligation(
      List<Identifier> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation,
      boolean cost,
      Optional<Obligation> parent) {
    this(
        new AnnotatingContext(contextIds, contextAnnotation), expression, annotation, cost, parent);
  }

  public Obligation(
      List<Identifier> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation) {
    this(contextIds, contextAnnotation, expression, annotation, Optional.empty());
  }

  public Obligation keepAnnotationAndCost(AnnotatingContext context, Expression expression) {
    return new Obligation(context, expression, annotation, cost, Optional.of(this));
  }

  public Obligation keepCost(
      AnnotatingContext context, Expression expression, Annotation annotation) {
    return new Obligation(context, expression, annotation, cost, Optional.of(this));
  }

  public Obligation keepContextAndAnnotationAndCost(Expression expression) {
    return new Obligation(context, expression, annotation, cost, Optional.of(this));
  }

  public Obligation substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new Obligation(
        context.substitute(solution), expression, annotation.substitute(solution), cost, parent);
  }

  public Map<String, Attribute> attributes(Prover.ProofVertexData proofVertexData) {
    final var rule = proofVertexData != null ? proofVertexData.schedule().rule() : null;
    final List<Constraint> generalConstraints =
        proofVertexData != null ? proofVertexData.generalConstraints() : List.of();
    final var haveGeneralConstraints = !generalConstraints.isEmpty();
    final var generalConstraintsString =
        !haveGeneralConstraints
            ? ""
            : generalConstraints.stream()
                .map(Constraint::toString)
                .collect(Collectors.joining("<br />"))
                .replace("\n", "<br />");

    final var obligationLabel =
        toString().replace("<", "&lt;").replace(">", "&gt;").replace("|", "&#124;");

    final var haveArgs = !proofVertexData.schedule().arguments().isEmpty();

    final var ruleName =
        (rule == null ? "?" : rule.getName())
            + (haveArgs
                ? ("<br/>"
                    + proofVertexData.schedule().arguments().entrySet().stream()
                        .map(Object::toString)
                        .collect(Collectors.joining(", ")))
                : "");

    final var constraintsLabel =
        (proofVertexData == null
            ? (new ArrayList<List<Constraint>>())
            : Util.truncate(
                proofVertexData.constraints().stream()
                    .map(
                        constraints ->
                            constraints.stream()
                                .map(Constraint::toString)
                                .map(x -> x.replace(">", "&gt;").replace("<", "&lt;"))
                                .collect(Collectors.joining("<br />"))
                                .replace("\n", "<br />"))
                    .collect(Collectors.joining("|")),
                16000));

    final var label =
        html(
            "{"
                + ("".equals(constraintsLabel) ? "" : constraintsLabel + "|")
                + (haveGeneralConstraints ? "General:<br/>" + generalConstraintsString + "|" : "")
                + "{"
                + obligationLabel
                + "|"
                + ruleName
                + "}}");

    return Map.of(
        "shape",
        new DefaultAttribute<>("record", AttributeType.STRING),
        "label",
        new NidiAttribute<>(label));
  }

  @Override
  public String toString() {
    final var idStr =
        context.getIds().isEmpty()
            ? "Ø"
            : context.getIds().stream().map(Object::toString).collect(joining(", "));
    final var contextStr =
        idStr + " | " + context.getAnnotation() + " " + context.getAnnotation().getNameAndId();

    return contextStr
        + "  ⊦"
        + Util.generateSubscript(cost ? 1 : 0)
        + "  "
        + expression
        + " | "
        + annotation
        + " "
        + annotation.getNameAndId();
  }
}
