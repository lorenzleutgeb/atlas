package xyz.leutgeb.lorenz.lac;

import static java.util.Optional.empty;
import static java.util.Optional.ofNullable;

import java.util.Optional;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.CombinedFunctionAnnotation;

@Value
class Config {
  public Optional<String> tactic;
  public Optional<CombinedFunctionAnnotation> annotation;

  public Config(Optional<String> tactic, Optional<CombinedFunctionAnnotation> annotation) {
    this.tactic = tactic;
    this.annotation = annotation;
  }

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
      result += annotation.get().withCost;
    } else {
      result += "infer";
    }
    return result;
  }
}
