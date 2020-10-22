package xyz.leutgeb.lorenz.lac;

import java.util.Optional;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;

@Value
final class Config {
  public final Optional<String> tactic;
  public final Optional<CombinedFunctionAnnotation> annotation;

  public Config(Optional<String> tactic, Optional<CombinedFunctionAnnotation> annotation) {
    this.tactic = tactic;
    this.annotation = annotation;
  }

  public static Config of(String tactic) {
    return new Config(Optional.ofNullable(tactic), Optional.empty());
  }

  public static Config of(String tactic, CombinedFunctionAnnotation annotation) {
    return new Config(Optional.ofNullable(tactic), Optional.ofNullable(annotation));
  }

  public static Config of(CombinedFunctionAnnotation annotation) {
    return new Config(Optional.empty(), Optional.ofNullable(annotation));
  }

  public static Object of() {
    return new Config(Optional.empty(), Optional.empty());
  }

  public boolean isUnknown() {
    return annotation.map(CombinedFunctionAnnotation::isUnknown).orElse(true);
  }

  @Override
  public String toString() {
    var result = "";
    if (tactic.isPresent()) {
      result += tactic.get();
    }
    if (tactic.isPresent() && annotation.isPresent()) {
      result += " ";
    }
    if (annotation.isPresent()) {
      result += annotation.get();
    }
    return result;
  }
}
