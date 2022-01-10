package xyz.leutgeb.lorenz.atlas.typing.resources.heuristics;

import java.util.Collection;
import java.util.List;
import xyz.leutgeb.lorenz.atlas.ast.Expression;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;

public interface AnnotationHeuristic {
  Annotation generate(String name, int size);

  default Annotation generate(int size) {
    return generate("Q", size);
  }

  default Annotation generate(String namePrefix, Annotation annotation) {
    return generate(namePrefix, annotation.size());
  }

  default Annotation generate(Annotation annotation) {
    return generate(annotation.size());
  }

  default Annotation generate(String namePrefix, Collection<?> collection) {
    return generate(namePrefix, collection.size());
  }

  default Annotation generate(Collection<?> collection) {
    return generate(collection.size());
  }

  default AnnotatingContext generateContext(String namePrefix, List<Identifier> ids) {
    return new AnnotatingContext(ids, generate(namePrefix, ids));
  }

  default AnnotatingContext generateContext(List<Identifier> ids) {
    return new AnnotatingContext(ids, generate(ids));
  }

  default Annotation generate(String namePrefix, Expression expression) {
    return generate(namePrefix, expression.getType() instanceof TreeType ? 1 : 0);
  }

  default Annotation generate(Expression expression) {
    return generate(expression.getType() instanceof TreeType ? 1 : 0);
  }
}
