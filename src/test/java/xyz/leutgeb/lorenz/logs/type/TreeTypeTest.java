package xyz.leutgeb.lorenz.logs.type;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class TreeTypeTest {

  @Test
  void equals() {
    Assertions.assertEquals(new TreeType(TypeVariable.ALPHA), new TreeType(TypeVariable.ALPHA));
  }
}
