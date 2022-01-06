package xyz.leutgeb.lorenz.atlas.typing.resources.proving;

import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;

import guru.nidi.graphviz.attribute.Label;
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

// TODO(lorenz.leutgeb): Maybe we need to navigate around obligations in a DAG/proof tree?
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

  public Map<String, Attribute> attributes(List<Constraint> generalConstraints) {
    final var generalConstraintsString =
        generalConstraints.isEmpty()
            ? ""
            : generalConstraints.stream()
                .map(Constraint::toStringWithReason)
                .collect(Collectors.joining("<br />"));
    return Map.of(
        "shape",
        new DefaultAttribute<>("none", AttributeType.STRING),
        "label",
        new NidiAttribute<>(
            Label.html(
                toString().replace("<", "&lt;").replace(">", "&gt;")
                    + (generalConstraints.isEmpty() ? "" : "<br />- - -<br />")
                    + generalConstraintsString)));
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
