package xyz.leutgeb.lorenz.lac.typing.resources.proving;

import static xyz.leutgeb.lorenz.lac.Util.bug;

import guru.nidi.graphviz.attribute.Label;
import java.util.List;
import java.util.Map;
import lombok.Value;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.lac.NidiAttribute;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.lac.typing.resources.Annotation;

// TODO: Maybe we need to navigate around obligations in a DAG/proof tree?
@Value
public class Obligation {
  // public static Obligation NOTHING = new Obligation();
  AnnotatingContext context;
  Expression expression;
  Annotation annotation;
  int cost;

  private Obligation() {
    context = null;
    expression = null;
    annotation = null;
    cost = -1;
  }

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
      List<String> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation) {
    this(contextIds, contextAnnotation, expression, annotation, 1);
  }

  public Obligation(
      List<String> contextIds,
      Annotation contextAnnotation,
      Expression expression,
      Annotation annotation,
      int cost) {
    this(new AnnotatingContext(contextIds, contextAnnotation), expression, annotation, cost);
  }

  public static Obligation nothing() {
    return new Obligation();
  }

  public boolean isNothing() {
    return context == null;
  }

  public Obligation keepCost(
      AnnotatingContext context, Expression expression, Annotation annotation) {
    return new Obligation(context, expression, annotation, cost);
  }

  public Obligation keepCostAndContext(Expression expression, Annotation annotation) {
    return new Obligation(context, expression, annotation, cost);
  }

  public Obligation keepCostAndContextAndAnnotation(Expression expression) {
    return new Obligation(context, expression, annotation, cost);
  }

  public Map<String, Attribute> attributes() {
    if (this.isNothing()) {
      return Map.of(
          "shape",
          new DefaultAttribute<>("none", AttributeType.STRING),
          "label",
          new DefaultAttribute<>("nothing", AttributeType.HTML));
    }
    return Map.of(
        "shape",
        new DefaultAttribute<>("none", AttributeType.STRING),
        "label",
        new NidiAttribute<>(Label.of(toString())));
  }

  @Override
  public String toString() {
    if (this.isNothing()) {
      return "nothing";
    }
    return context
        + " ⊦"
        + Util.generateSubscript(cost)
        + " "
        + expression
        + " | "
        + annotation.getName();
  }
}
