package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.*;
import static java.util.stream.Collectors.toList;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.BiFunction;
import java.util.stream.Stream;
import javax.annotation.Nonnull;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public interface Rule extends BiFunction<Obligation, AnnotatingGlobals, Rule.ApplicationResult> {
  @Value
  class ApplicationResult {
    @Nonnull @NonNull List<Obligation> obligations;
    @Nonnull @NonNull List<List<Constraint>> constraints;
    @Nonnull @NonNull List<Constraint> generalConstraints;

    public ApplicationResult(
        List<Obligation> obligations,
        List<List<Constraint>> constraints,
        List<Constraint> generalConstraints) {
      if (obligations.size() != constraints.size()) {
        throw new IllegalArgumentException(
            "obligations.size() and constraints.size() must be equal");
      }
      this.obligations = unmodifiableList(obligations);
      this.constraints = unmodifiableList(constraints);
      this.generalConstraints = unmodifiableList(generalConstraints);
    }

    public static ApplicationResult empty() {
      return new ApplicationResult(emptyList(), emptyList(), emptyList());
    }

    public static ApplicationResult onlyConstraints(List<Constraint> constraints) {
      if (constraints.isEmpty()) {
        return empty();
      }
      return new ApplicationResult(
          singletonList(Obligation.nothing()), singletonList(constraints), emptyList());
    }

    public static ApplicationResult onlyObligations(List<Obligation> obligations) {
      return new ApplicationResult(
          obligations,
          Stream.generate(Collections::<Constraint>emptyList)
              .limit(obligations.size())
              .collect(toList()),
          emptyList());
    }

    public void collectInto(Collection<Constraint> constraints) {
      constraints.addAll(generalConstraints);
      this.constraints.forEach(constraints::addAll);
    }
  }
}
