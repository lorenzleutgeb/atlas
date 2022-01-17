package xyz.leutgeb.lorenz.atlas;

import java.io.IOException;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;

public class OrderTest {
  @Test
  public void test() throws TypeError, IOException {
    final var program = TestUtil.loadAndNormalizeAndInferAndUnshare("Order.e", "Order.f");
  }
}
