package xyz.leutgeb.lorenz.lac.typing.resources;

import java.util.Map;
import java.util.Set;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Value;
import org.jgrapht.Graph;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

@Value
public class AnnotatingGlobals {
  @Getter(value = AccessLevel.NONE)
  Map<String, FunctionAnnotation> functionAnnotations;

  @Getter(value = AccessLevel.NONE)
  Map<String, Set<FunctionAnnotation>> costFreeFunctionAnnotations;

  AnnotationHeuristic heuristic;

  Graph<Identifier, SizeEdge> sizeAnalysis;

  public AnnotatingGlobals(
      Map<String, FunctionAnnotation> functionAnnotations,
      Map<String, Set<FunctionAnnotation>> costFreeFunctionAnnotations,
      Graph<Identifier, SizeEdge> sizeAnalysis,
      AnnotationHeuristic heuristic) {
    this.costFreeFunctionAnnotations = costFreeFunctionAnnotations;
    this.functionAnnotations = functionAnnotations;
    this.sizeAnalysis = sizeAnalysis;
    this.heuristic = heuristic;
  }

  public AnnotatingGlobals(
      Map<String, FunctionAnnotation> functionAnnotations,
      Map<String, Set<FunctionAnnotation>> costFreeFunctionAnnotations,
      Graph<Identifier, SizeEdge> sizeAnalysis) {
    this(
        functionAnnotations,
        costFreeFunctionAnnotations,
        sizeAnalysis,
        SmartRangeHeuristic.DEFAULT);
  }

  public void addFunctionAnnotation(
      String name, FunctionAnnotation withCost, Set<FunctionAnnotation> costFrees) {
    if (!costFrees.stream()
        .allMatch(
            costFree -> {
              if (withCost.to().size() > 1 || costFree.to().size() > 1) {
                return false;
                // throw new IllegalArgumentException("annotation of result can be of size one at
                // most");
              }
              if (withCost.from().size() != costFree.from().size()) {
                return false;
                // throw new IllegalArgumentException("size mismatch");
              }
              if (withCost.to().size() != costFree.to().size()) {
                return false;
                // throw new IllegalArgumentException("size mismatch");
              }
              return true;
            })) {
      throw new IllegalArgumentException();
    }
    functionAnnotations.put(name, withCost);
    costFreeFunctionAnnotations.put(name, costFrees);
  }

  public FunctionAnnotation getSignature(String name) {
    return functionAnnotations.get(name);
  }

  public Set<FunctionAnnotation> getCostFreeSignatures(String name) {
    return costFreeFunctionAnnotations.get(name);
  }
}
