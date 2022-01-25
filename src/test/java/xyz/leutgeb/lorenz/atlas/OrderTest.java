package xyz.leutgeb.lorenz.atlas;

import java.io.IOException;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.atlas.module.Loader;
import xyz.leutgeb.lorenz.atlas.typing.simple.TypeError;

/**
 * f depends on g e depends on h g and h are mutually recursive, part of the same SCC h additionally
 * depends on i
 *
 * <p>e -> h -> i || f -> g -> j
 *
 * <p>Correct ordering: 1.a. i 1.b. j 2. h and g 3.a. e 3.b. f (a/b are interchangable)
 */
public class OrderTest {
  private static final String FIXTURE =
      """
f x = (f (g x))

e x = (h x)

g x = (h (j x))

h x = (g (i x))

i x = x

j x = x
          """;

  @Test
  public void test() throws TypeError, IOException {
    final var program = Loader.atCurrentWorkingDirectory().loadInline(FIXTURE);
    program.normalize();
    program.infer();
    program.printAllSimpleSignaturesInOrder(System.out);
  }
}
