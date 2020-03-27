package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Collectors;
import javax.annotation.Nonnull;
import lombok.NonNull;
import lombok.Value;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public interface Rule
    extends BiFunction<Obligation, AnnotatingGlobals, Rule.RuleApplicationResult> {
  @Value
  class RuleApplicationResult {
    @Nonnull @NonNull List<Pair<Obligation, List<Constraint>>> obligations;
    @Nonnull @NonNull List<Constraint> generalConstraints;

    public static RuleApplicationResult empty() {
      return new RuleApplicationResult(emptyList(), emptyList());
    }

    public static RuleApplicationResult onlyConstraints(List<Constraint> constraints) {
      return new RuleApplicationResult(
          singletonList(Pair.create(Obligation.nothing(), constraints)), emptyList());
    }

    public static RuleApplicationResult onlyObligations(List<Obligation> obligations) {
      return new RuleApplicationResult(
          obligations.stream()
              .map(k -> Pair.create(k, (List<Constraint>) new ArrayList<Constraint>()))
              .collect(Collectors.toList()),
          emptyList());
    }
  }
}
