package xyz.leutgeb.lorenz.lac.typing.resources.proving;

import static java.util.stream.Collectors.joining;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;

import guru.nidi.graphviz.attribute.Label;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import lombok.Value;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.util.NidiAttribute;
import xyz.leutgeb.lorenz.lac.util.Util;

// TODO(lorenz.leutgeb): Maybe we need to navigate around obligations in a DAG/proof tree?
@Value
public class Obligation {
  AnnotatingContext context;
  Expression expression;
  Annotation annotation;
  int cost;

  public Obligation(
      AnnotatingContext context, Expression expression, Annotation annotation, int cost) {
    this.context = context;
    this.expression = expression;

    if (annotation.size() > 1) {
      throw bug(
          "expression being judged cannot be annotated with an annotation that has more than one element");
    }

    this.annotation = annotation;
    if (cost != 0 && cost != 1) {
      throw new IllegalArgumentException("cost must be 0 or 1");
    }
    this.cost = cost;
  }

  public Obligation(
      List<Identifier> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation) {
    this(contextIds, contextAnnotation, expression, annotation, 1);
  }

  public Obligation(
      List<Identifier> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation,
      int cost) {
    this(new AnnotatingContext(contextIds, contextAnnotation), expression, annotation, cost);
  }

  public Obligation keepAnnotationAndCost(AnnotatingContext context, Expression expression) {
    return new Obligation(context, expression, annotation, cost);
  }

  public Obligation keepCost(
      AnnotatingContext context, Expression expression, Annotation annotation) {
    return new Obligation(context, expression, annotation, cost);
  }

  public Obligation keepContextAndAnnotationAndCost(Expression expression) {
    return new Obligation(context, expression, annotation, cost);
  }

  public Obligation substitute(Map<Coefficient, KnownCoefficient> solution) {
    return new Obligation(
        context.substitute(solution), expression, annotation.substitute(solution), cost);
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
                toString()
                    + (generalConstraints.isEmpty() ? "" : "<br />- - -<br />")
                    + generalConstraintsString)));
  }

  @Override
  public String toString() {
    final var idStr =
        context.getIds().isEmpty()
            ? "Ø"
            : context.getIds().stream().map(Object::toString).collect(joining(", "));
    ;
    final var contextStr =
        idStr
            + " | "
            + context.getAnnotation()
            + " <i style=\"color:blue;\">"
            + context.getAnnotation().getNameAndId()
            + "</i>";

    return contextStr
        + "  ⊦"
        + Util.generateSubscript(cost)
        + "  "
        + expression
        + " | "
        + annotation
        + " <i style=\"color:blue;\">"
        + annotation.getNameAndId()
        + "</i>";
  }
}
