package xyz.leutgeb.lorenz.atlas;

import java.io.IOException;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;
import xyz.leutgeb.lorenz.atlas.unification.UnificationError;

public class OrderTest {
  @Test
  public void test() throws UnificationError, TypeError, IOException {
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare("Order.e", "Order.f");
  }
}
