package xyz.leutgeb.lorenz.atlas;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.atlas.TestUtil.loadAndNormalizeAndInferAndUnshare;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;

public class AutomationTest {
  public void test(String fqn, CombinedFunctionAnnotation combinedFunctionAnnotation)
      throws UnificationError, TypeError, IOException {
    final var program = loadAndNormalizeAndInferAndUnshare(fqn);
    final var annotations = Map.of(fqn, combinedFunctionAnnotation);
    final var result =
        program.solve(annotations, Collections.emptyMap(), true, false, Collections.emptySet());
    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
