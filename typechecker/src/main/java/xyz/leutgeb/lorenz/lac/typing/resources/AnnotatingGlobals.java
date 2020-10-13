package xyz.leutgeb.lorenz.lac.typing.resources;

import static java.util.Collections.emptyMap;

import java.util.Map;
import java.util.Set;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.Value;
import org.jgrapht.Graph;
import org.jgrapht.graph.DirectedAcyclicGraph;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.AnnotationHeuristic;
import xyz.leutgeb.lorenz.lac.typing.resources.heuristics.SmartRangeHeuristic;
import xyz.leutgeb.lorenz.lac.util.SizeEdge;

@Value
public class AnnotatingGlobals {
  @Getter(value = AccessLevel.NONE)
  Map<String, CombinedFunctionAnnotation> functionAnnotations;

  AnnotationHeuristic heuristic;

  Graph<Identifier, SizeEdge> sizeAnalysis;

  public AnnotatingGlobals(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Graph<Identifier, SizeEdge> sizeAnalysis,
      AnnotationHeuristic heuristic) {
    this.functionAnnotations = functionAnnotations;
    this.sizeAnalysis = sizeAnalysis;
    this.heuristic = heuristic;
  }

  public AnnotatingGlobals(
      Map<String, CombinedFunctionAnnotation> functionAnnotations,
      Graph<Identifier, SizeEdge> sizeAnalysis) {
    this(functionAnnotations, sizeAnalysis, SmartRangeHeuristic.DEFAULT);
  }

  public static AnnotatingGlobals empty() {
    return new AnnotatingGlobals(emptyMap(), new DirectedAcyclicGraph<>(SizeEdge.class));
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
    functionAnnotations.put(name, new CombinedFunctionAnnotation(withCost, costFrees));
  }

  public CombinedFunctionAnnotation getSignature(String name) {
    return functionAnnotations.get(name);
  }
}
