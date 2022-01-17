package xyz.leutgeb.lorenz.atlas.typing.resources.heuristics;

import java.util.Collection;
import java.util.List;
import xyz.leutgeb.lorenz.atlas.ast.expressions.Expression;
import xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.atlas.typing.resources.Annotation;

public interface AnnotationHeuristic {
  Annotation generate(String namePrefix, int size);

  Annotation generate(String namePrefix, Annotation shape);

  default Annotation generate(int size) {
    return generate("Q", size);
  }

  default Annotation generate(Annotation annotation) {
    return generate("Q", annotation);
  }

  default Annotation generate(String namePrefix, Collection<?> collection) {
    return generate(namePrefix, collection.size());
  }

  default Annotation generate(Collection<?> collection) {
    return generate(collection.size());
  }

  default AnnotatingContext generateContext(String namePrefix, List<IdentifierExpression> ids) {
    return new AnnotatingContext(ids, generate(namePrefix, ids));
  }

  default AnnotatingContext generateContext(List<IdentifierExpression> ids) {
    return new AnnotatingContext(ids, generate(ids));
  }

  default Annotation generate(String namePrefix, Expression expression) {
    return generate(namePrefix, expression.getType().countTrees().get());
  }

  default Annotation generate(Expression expression) {
    return generate(expression.getType().countTrees().get());
  }
}
