package xyz.leutgeb.lorenz.lac.util;

import lombok.Value;

@Value
public class Fraction {
  public static final Fraction ZERO = new Fraction(0);
  public static final Fraction ONE = new Fraction(1);
  public static final Fraction TWO = new Fraction(2);
  public static final Fraction THREE = new Fraction(3);

  int numerator;
  int denominator;

  public Fraction(int numerator) {
    this(numerator, 1);
  }

  public Fraction(int numerator, int denominator) {
    if (denominator == 0) {
      throw new IllegalArgumentException("cannot represent infinity");
    }
    if (denominator < 0) {
      numerator = -numerator;
    }
    this.numerator = numerator;
    this.denominator = denominator;
  }

  public Fraction negate() {
    return new Fraction(-numerator, denominator);
  }

  @Override
  public String toString() {
    String str = null;
    if (denominator == 1) {
      str = Integer.toString(numerator);
    } else if (numerator == 0) {
      str = "0";
    } else {
      str = numerator + " / " + denominator;
    }
    return str;
  }
}
