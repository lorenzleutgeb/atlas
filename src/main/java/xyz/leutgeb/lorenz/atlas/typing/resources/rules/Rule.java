package xyz.leutgeb.lorenz.atlas.typing.resources.rules;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.unmodifiableList;
import static java.util.stream.Collectors.toList;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.proving.Obligation;

public interface Rule extends BiFunction<Obligation, AnnotatingGlobals, Rule.ApplicationResult> {
  @Override
  default ApplicationResult apply(Obligation obligation, AnnotatingGlobals globals) {
    return apply(obligation, globals, emptyMap());
  }

  ApplicationResult apply(
      Obligation obligation, AnnotatingGlobals globals, Map<String, String> arguments);

  default String getName() {
    String text = this.getClass().getSimpleName();
    Matcher m = Pattern.compile("(^[A-Z]|[a-z])[A-Z]").matcher(text);

    StringBuilder sb = new StringBuilder();
    int last = 0;
    while (m.find()) {
      sb.append(text, last, m.start());
      sb.append(m.group(0).charAt(0));
      sb.append(":");
      sb.append(Character.toLowerCase(m.group(0).charAt(1)));
      last = m.end();
    }
    sb.append(text.substring(last));

    return sb.toString().toLowerCase();
  }

  record ApplicationResult(
      List<Obligation> obligations,
      List<List<Constraint>> constraints,
      List<Constraint> generalConstraints) {
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

    public static ApplicationResult noop(Obligation obligation) {
      return new ApplicationResult(List.of(obligation), List.of(List.of()), List.of());
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
