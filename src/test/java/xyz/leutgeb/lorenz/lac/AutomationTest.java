package xyz.leutgeb.lorenz.lac;

import static org.junit.jupiter.api.Assertions.assertTrue;
import static xyz.leutgeb.lorenz.lac.TestUtil.loadAndNormalizeAndInferAndUnshare;

import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class AutomationTest {
  public void test(String fqn, CombinedFunctionAnnotation combinedFunctionAnnotation)
      throws UnificationError, TypeError, IOException {
    final var program = loadAndNormalizeAndInferAndUnshare(fqn);
    final var annotations = Map.of(fqn, combinedFunctionAnnotation);
    final var result =
        program.solve(annotations, Collections.emptyMap(), true, Collections.emptySet());
    assertTrue(result.isSatisfiable());
    program.printAllInferredSignaturesInOrder(System.out);
  }
}
