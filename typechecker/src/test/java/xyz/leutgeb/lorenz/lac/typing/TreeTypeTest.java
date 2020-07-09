package xyz.leutgeb.lorenz.lac.typing;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import xyz.leutgeb.lorenz.lac.typing.simple.TypeVariable;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;

class TreeTypeTest {
  @Test
  void equals() {
    Assertions.assertEquals(new TreeType(TypeVariable.ALPHA), new TreeType(TypeVariable.ALPHA));
  }
}
