package xyz.leutgeb.lorenz.atlas.typing.resources.heuristics;

import java.util.Collection;
import java.util.List;
import xyz.leutgeb.lorenz.atlas.ast.Expression;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;

public interface AnnotationHeuristic {
  Annotation generate(String namePrefix, int size);

  default Annotation generate(String namePrefix, Annotation annotation) {
    return generate(namePrefix, annotation.size());
  }

  default Annotation generate(String namePrefix, Collection<?> collection) {
    return generate(namePrefix, collection.size());
  }

  default AnnotatingContext generateContext(String namePrefix, List<Identifier> ids) {
    return new AnnotatingContext(ids, generate(namePrefix, ids));
  }

  default Annotation generate(String namePrefix, Expression expression) {
    return generate(namePrefix, expression.getType() instanceof TreeType ? 1 : 0);
  }
}
