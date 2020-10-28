package xyz.leutgeb.lorenz.lac.util;

import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.Test;

class FractionTest {
  @Test
  public void fancyToString() {
    System.out.println(new Fraction(7, 8).toString());
    System.out.println(new Fraction(-7, 8).toString());
  }
}
