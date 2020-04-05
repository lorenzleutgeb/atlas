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

public interface Rule extends BiFunction<Obligation, AnnotatingGlobals, Rule.ApplicationResult> {
  @Value
  class ApplicationResult {
    @Nonnull @NonNull List<Pair<Obligation, List<Constraint>>> obligations;
    @Nonnull @NonNull List<Constraint> generalConstraints;

    public static ApplicationResult empty() {
      return new ApplicationResult(emptyList(), emptyList());
    }

    public static ApplicationResult onlyConstraints(List<Constraint> constraints) {
      return new ApplicationResult(
          singletonList(Pair.create(Obligation.nothing(), constraints)), emptyList());
    }

    public static ApplicationResult onlyObligations(List<Obligation> obligations) {
      return new ApplicationResult(
          obligations.stream()
              .map(k -> Pair.create(k, (List<Constraint>) new ArrayList<Constraint>()))
              .collect(Collectors.toList()),
          emptyList());
    }
  }
}
