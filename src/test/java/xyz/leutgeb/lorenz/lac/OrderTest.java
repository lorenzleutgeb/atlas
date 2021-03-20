package xyz.leutgeb.lorenz.lac;

import java.io.IOException;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeError;
import xyz.leutgeb.lorenz.lac.unification.UnificationError;

public class OrderTest {
  @Test
  public void test() throws UnificationError, TypeError, IOException {
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare("Order.e", "Order.f");
  }
}
