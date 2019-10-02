package xyz.leutgeb.lorenz.logs.typing;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;

class TreeTypeTest {

  @Test
  void equals() {
    Assertions.assertEquals(new TreeType(TypeVariable.ALPHA), new TreeType(TypeVariable.ALPHA));
  }
}
