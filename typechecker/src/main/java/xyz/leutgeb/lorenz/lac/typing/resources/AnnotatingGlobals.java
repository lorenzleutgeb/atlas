package xyz.leutgeb.lorenz.lac.typing.resources;

import java.util.Map;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Value;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.lac.SizeEdge;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;

@Value
public class AnnotatingGlobals {
  @Getter(value = AccessLevel.NONE)
  Map<String, FunctionAnnotation> functionAnnotations;

  @Getter(value = AccessLevel.NONE)
  Map<String, FunctionAnnotation> costFreeFunctionAnnotations;

  AnnotationHeuristic heuristic;

  Graph<Identifier, SizeEdge> sizeAnalysis;

  public AnnotatingGlobals(
      Map<String, FunctionAnnotation> functionAnnotations,
      Map<String, FunctionAnnotation> costFreeFunctionAnnotations,
      Graph<Identifier, SizeEdge> sizeAnalysis,
      AnnotationHeuristic heuristic) {
    this.costFreeFunctionAnnotations = costFreeFunctionAnnotations;
    this.functionAnnotations = functionAnnotations;
    this.sizeAnalysis = sizeAnalysis;
    this.heuristic = heuristic;
  }

  public AnnotatingGlobals(
      Map<String, FunctionAnnotation> functionAnnotations,
      Map<String, FunctionAnnotation> costFreeFunctionAnnotations,
      Graph<Identifier, SizeEdge> sizeAnalysis) {
    this(
        functionAnnotations,
        costFreeFunctionAnnotations,
        sizeAnalysis,
        SmartRangeHeuristic.DEFAULT);
  }

  public void addFunctionAnnotation(
      String name, FunctionAnnotation withCost, FunctionAnnotation costFree) {
    if (withCost.to().size() > 1 || costFree.to().size() > 1) {
      throw new IllegalArgumentException("annotation of result can be of size one at most");
    }
    if (withCost.from().size() != costFree.from().size()) {
      throw new IllegalArgumentException("size mismatch");
    }
    if (withCost.to().size() != costFree.to().size()) {
      throw new IllegalArgumentException("size mismatch");
    }
    functionAnnotations.put(name, withCost);
    costFreeFunctionAnnotations.put(name, costFree);
  }

  public FunctionAnnotation getDependingOnCost(String name, int cost) {
    if (cost == 0) {
      return costFreeFunctionAnnotations.get(name);
    } else if (cost == 1) {
      return functionAnnotations.get(name);
    }
    throw new IllegalStateException("cost is neither one nor zero");
  }
}
