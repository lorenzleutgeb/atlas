package xyz.leutgeb.lorenz.atlas;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;

import java.util.Optional;
import xyz.leutgeb.lorenz.atlas.typing.resources.CombinedFunctionAnnotation;

record Config(Optional<String> tactic, Optional<CombinedFunctionAnnotation> annotation) {
  public static Config of(String tactic) {
    return new Config(ofNullable(tactic), empty());
  }

  public static Config of(String tactic, CombinedFunctionAnnotation annotation) {
    return new Config(ofNullable(tactic), ofNullable(annotation));
  }

  public static Config of(CombinedFunctionAnnotation annotation) {
    return new Config(empty(), ofNullable(annotation));
  }

  public static Config of() {
    return new Config(empty(), empty());
  }

  public boolean isUnknown() {
    return annotation.map(CombinedFunctionAnnotation::isUnknown).orElse(true);
  }

  @Override
  public String toString() {
    var result = "";
    if (tactic.isPresent()) {
      result += tactic.get();
    } else {
      result += "auto";
    }
    result += " ";
    if (annotation.isPresent()) {
      result += annotation.get();
    } else {
      result += "infer";
    }
    return result;
  }
}
