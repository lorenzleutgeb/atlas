package xyz.leutgeb.lorenz.atlas.typing.resources.heuristics;

import java.util.ArrayList;
import java.util.List;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;

public class SimpleFunctionHeuristic implements AnnotationHeuristic {
  public static final SimpleFunctionHeuristic DEFAULT = new SimpleFunctionHeuristic();

  public Annotation generateWithoutConstant(String namePrefix, int size) {
    return new Annotation(size, shape(size, 0), namePrefix);
  }

  public Annotation generateWithConstant(String namePrefix, int size) {
    var shape = shape(size, 1);
    shape.add(Annotation.unitIndex(size));
    return new Annotation(size, shape, namePrefix);
  }

  private List<List<Integer>> shape(int size, int excessCapacity) {
    List<List<Integer>> shape = new ArrayList<>(size + excessCapacity);
    for (int i = 0; i < size; i++) {
      final List<Integer> log = new ArrayList<>(size + 1);
      for (int j = 0; j < size; j++) {
        log.add(i == j ? 1 : 0);
      }
      log.add(0);
      shape.add(log);
    }
    return shape;
  }

  @Override
  public Annotation generate(String namePrefix, int size) {
    return generateWithConstant(namePrefix, size);
  }

  @Override
  public Annotation generate(String namePrefix, Annotation shape) {
    throw new UnsupportedOperationException();
  }
}
