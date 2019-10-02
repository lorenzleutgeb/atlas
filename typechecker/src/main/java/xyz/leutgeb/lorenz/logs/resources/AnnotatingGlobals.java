package xyz.leutgeb.lorenz.logs.resources;

import java.util.Map;
import lombok.AllArgsConstructor;
import lombok.Value;
import org.hipparchus.util.Pair;

@Value
@AllArgsConstructor
public class AnnotatingGlobals {
  Map<String, Pair<Annotation, Annotation>> functionAnnotations;
  Constraints constraints;
  int cost;

  public AnnotatingGlobals costFree() {
    if (cost == 0) {
      return this;
    }
    return new AnnotatingGlobals(functionAnnotations, constraints, 0);
  }
}
