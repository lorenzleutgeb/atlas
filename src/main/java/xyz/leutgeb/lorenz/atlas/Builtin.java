package xyz.leutgeb.lorenz.atlas;

import java.util.Random;

public class Builtin {
  private static final Random RANDOM = new Random(0);

  public static boolean coin() {
    return RANDOM.nextBoolean();
  }
}
