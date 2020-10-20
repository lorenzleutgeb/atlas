package xyz.leutgeb.lorenz.lac.ast;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode
public class SystemIntro implements Intro {
  public static final SystemIntro INSTANCE = new SystemIntro();

  @Override
  public String toString() {
    return "<system>";
  }
}
