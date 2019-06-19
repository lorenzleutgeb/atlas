package xyz.leutgeb.lorenz.logs.resources;

import java.util.Map;
import lombok.Value;
import org.hipparchus.util.Pair;

@Value
public class AnnotatingGlobals {
  Map<String, Pair<Annotation, Annotation>> functionAnnotations;
  Constraints constraints;
}
