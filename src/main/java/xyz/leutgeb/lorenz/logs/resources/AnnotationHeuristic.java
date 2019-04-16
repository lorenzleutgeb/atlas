package xyz.leutgeb.lorenz.logs.resources;

public interface AnnotationHeuristic {
  TypingContextAnnotation generate(int size, Constraints context);
}
