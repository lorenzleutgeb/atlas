package xyz.leutgeb.lorenz.lac.typing.resources.rules;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.unmodifiableList;
import static java.util.stream.Collectors.toList;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.stream.Stream;
import lombok.NonNull;
import lombok.Value;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.proving.Obligation;

public interface Rule extends BiFunction<Obligation, AnnotatingGlobals, Rule.ApplicationResult> {
  @Override
  default ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    return apply(obligation, globals, emptyMap());
  }

  ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments);

  String getName();

  @Value
  class ApplicationResult {
    @NonNull List<Obligation> obligations;
    @NonNull List<List<Constraint>> constraints;
    @NonNull List<Constraint> generalConstraints;

    public ApplicationResult(List<Obligation> obligations, List<List<Constraint>> constraints) {
      this(obligations, constraints, emptyList());
    }

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
      return new ApplicationResult(emptyList(), emptyList(), constraints);
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
