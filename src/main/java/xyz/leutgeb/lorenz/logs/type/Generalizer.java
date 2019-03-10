package xyz.leutgeb.lorenz.logs.type;

import java.util.HashMap;
import xyz.leutgeb.lorenz.logs.unification.UnificationVariable;

public class Generalizer {
  private final HashMap<UnificationVariable, String> mapping;

  private static final String[] GREEK =
      new String[] {
        "α", "β", "γ", "δ", "ε", "λ", "μ", "ν", "π", "ω", "ρ", "σ", "τ", "ξ",
      };

  public Generalizer() {
    this.mapping = new HashMap<>(GREEK.length);
  }

  public Type generalize(UnificationVariable u) {
    return new TypeVar(
        this.mapping.computeIfAbsent(
            u,
            k -> {
              var x = this.mapping.size();
              if (x < GREEK.length) {
                return GREEK[x];
              }
              return "gen" + x;
            }));
  }
}
