package xyz.leutgeb.lorenz.lac.typing.resources.proving;

import static java.util.Collections.emptyList;
import static java.util.Collections.singletonList;
import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.stack;
import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;

import com.google.common.collect.Streams;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.lac.NidiAttribute;
import xyz.leutgeb.lorenz.lac.NidiExporter;
import xyz.leutgeb.lorenz.lac.Util;
import xyz.leutgeb.lorenz.lac.ast.*;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.*;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;

@Slf4j
public class Prover {
  private static final Rule letTreeCfRule = LetTreeCf::apply;
  private static final Rule letGenRule = LetGen::apply;
  private static final Rule letTreeRule = LetTree::apply;
  private static final Rule applicationRule = App::apply;
  private static final Rule comparisonRule = Cmp::apply;
  private static final Rule ifThenElseRule = Ite::apply;
  private static final Rule matchRule = Match::apply;
  private static final Rule nodeRule = Node::apply;
  private static final Rule shareRule = Share::apply;
  private static final Rule variableRule = Var::apply;
  private static final Rule weakenVariableRule = WVar::apply;
  private static final Rule weakenRule = W::apply;
  private static final Rule leafRule = Leaf::apply;

  private static final boolean DEFAULT_WEAKEN = false;

  private final String name;
  private final Path basePath;

  @Getter
  private final DirectedAcyclicGraph<Obligation, DefaultEdge> proof =
      new DirectedAcyclicGraph<>(DefaultEdge.class);

  private final Map<DefaultEdge, Map<String, Attribute>> edgeAttributes = new HashMap<>();
  private final Map<Obligation, Map<String, Attribute>> vertexAttributes = new HashMap<>();

  @Getter private final Set<Constraint> accumulatedConstraints = new HashSet<>();

  // TODO: Find a better way to handle globals...
  @Getter @Setter private AnnotatingGlobals globals;

  @Getter @Setter private boolean weaken = DEFAULT_WEAKEN;

  public Prover(String name, AnnotatingGlobals globals, Path basePath) {
    this.name = name;
    this.globals = globals;
    this.basePath = basePath;
  }

  public Prover(String name, AnnotatingGlobals globals) {
    this(name, globals, Paths.get("out"));
  }

  /** Chooses which rules should be applied (in order) to prove given obligation. */
  private Stack<Rule> chooseRules(Obligation obligation) {
    if (obligation.isNothing()) {
      return stack();
    }

    // TODO: Be smart about the application of (w:var) and (w). (w) should probably be applied
    // maximizing the knowledge about identifiers in the context. This means that (w:var) should
    // not be applied in some cases since it removes identifiers from the context that might carry
    // otherwise useful information?!

    final var expression = obligation.getExpression();
    if (!WVar.redundantIds(obligation).isEmpty()) {
      return stack(weakenVariableRule);
    }
    if (expression.isTerminal()) {
      if (weaken) {
        log.info(
            "Scheduling (w) for application to terminal expression {} because weakening is enabled!",
            expression);
        return stack(weakenRule, chooseRule(expression));
      } else {
        log.info(
            "Not scheduling (w) for application to terminal expression {} because weakening is disabled.",
            expression);
        return stack(chooseRule(expression));
      }
    } else {
      return stack(chooseRule(expression));
    }
  }

  /** Chooses which rule should be applied next to given expression. */
  private static Rule chooseRule(Expression e) {
    if (e instanceof BooleanExpression) {
      return comparisonRule;
    } else if (e instanceof Tuple) {
      return nodeRule;
    } else if (e instanceof CallExpression) {
      return applicationRule;
    } else if (e instanceof Identifier) {
      if (LEAF.equals(e)) {
        return leafRule;
      } else {
        return variableRule;
      }
    } else if (e instanceof IfThenElseExpression) {
      return ifThenElseRule;
    } else if (e instanceof MatchExpression) {
      return matchRule;
    } else if (e instanceof LetExpression) {
      if (((LetExpression) e).getValue().getType() instanceof TreeType) {
        // TODO: When should we apply the cost-free version?
        return letTreeRule;
      } else {
        return letGenRule;
      }
    } else if (e instanceof ShareExpression) {
      return shareRule;
    } else {
      throw bug(
          "could not choose a rule for expression of type " + e.getClass().getCanonicalName());
    }
  }

  public Obligation share(Obligation obligation) {
    while (obligation.getExpression() instanceof ShareExpression) {
      final var result = apply(obligation, shareRule);
      obligation = result.getObligations().get(0);
    }
    return obligation;
  }

  public List<Obligation> proveUntilExpressionEquals(Obligation obligation, Expression expression) {
    return proveUntil(obligation, x -> expression.equals(x.getExpression()));
  }

  public List<Obligation> proveUntil(Obligation obligation, Predicate<Obligation> condition) {
    if (obligation == null) {
      return emptyList();
    }

    // If the given obligation matches, stop immediately!
    if (condition.test(obligation)) {
      return singletonList(obligation);
    }

    final Stack<Rule> rules = chooseRules(obligation);
    if (rules.isEmpty()) {
      return emptyList();
    } else if (rules.size() == 1) {
      final Rule rule = rules.pop();
      final var ruleResult = apply(obligation, rule);
      return ruleResult.getObligations().stream()
          .flatMap(x -> proveUntil(x, condition).stream())
          .collect(Collectors.toList());
    }

    Rule.ApplicationResult ruleResult = null;
    while (!rules.isEmpty()) {
      final Rule rule = rules.pop();
      ruleResult = apply(obligation, rule);
      if (!ruleResult.getObligations().isEmpty()) {
        if (ruleResult.getObligations().size() > 1) {
          throw bug(
              "if there are multiple rule applications scheduled, all of them except the last must return exactly one obligation");
        }
        obligation = ruleResult.getObligations().get(0);
      } else {
        if (!rules.isEmpty()) {
          throw bug("multiple rule applications were scheduled but we ran out of obligations");
        }
      }
    }
    return ruleResult.getObligations().stream()
        .flatMap(x -> proveUntil(x, condition).stream())
        .collect(Collectors.toList());
    /*
    else if (rules.size() != 1) {
      log.info("proveOneStep will not apply a multi-rule choice to " + obligation);
      return singletonList(obligation);
    }

    if (rule.equals(weakenRule)) {
      log.info("proveOneStep will not apply (w) rule to " + obligation);
      return singletonList(obligation);
    }
     */

  }

  public Rule.ApplicationResult apply(Obligation obligation, Rule rule) {
    final var result = rule.apply(obligation, globals);
    if (result == null) {
      throw bug("typing rule implementation returned null");
    }
    ingest(obligation, result);
    return result;
  }

  public void prove(List<Obligation> obligations) {
    obligations.forEach(this::prove);
  }

  /** Translates given obligation into a set of constraints that would prove given obligation. */
  public void prove(Obligation obligation) {
    Stack<Obligation> obligations = new Stack<>();
    obligations.push(obligation);
    proof.addVertex(obligation);
    vertexAttributes.put(obligation, obligation.attributes());

    while (!obligations.isEmpty()) {
      final var top = obligations.pop();
      final var rules = chooseRules(top);

      var nextObligation = top;
      Rule.ApplicationResult ruleResult = null;

      while (!rules.isEmpty()) {
        final var rule = rules.pop();
        ruleResult = apply(nextObligation, rule);

        if (!rules.isEmpty()) {
          if (ruleResult.getObligations().size() > 1) {
            throw bug(
                "if there are multiple rule applications scheduled, all of them except the last must return exactly one obligation");
          }

          if (ruleResult.getObligations().isEmpty()) {
            throw bug("multiple rule applications were scheduled but we ran out of obligations");
          }

          nextObligation = ruleResult.getObligations().get(0);
        } else {
          obligations.addAll(ruleResult.getObligations());
        }
      }
    }
  }

  private void ingest(Obligation previous, Rule.ApplicationResult ruleResult) {
    if (!proof.containsVertex(previous)) {

      proof.addVertex(previous);
      vertexAttributes.put(previous, previous.attributes());
    }

    ruleResult
        .getObligations()
        .forEach(
            o -> {
              proof.addVertex(o);
              vertexAttributes.put(o, o.attributes());
            });
    final var lele = ruleResult.getGeneralConstraints();
    Streams.zip(
            ruleResult.getObligations().stream(), ruleResult.getConstraints().stream(), Pair::of)
        .forEach(
            o -> {
              final var edge = proof.addEdge(previous, o.getLeft());
              final var lel =
                  o.getRight().stream().map(Object::toString).collect(Collectors.joining("<br />"))
                      + (lele.isEmpty()
                          ? ""
                          : "<br /> - - - - <br />"
                              + lele.stream()
                                  .map(Object::toString)
                                  .collect(Collectors.joining("<br />")));
              edgeAttributes.put(
                  edge,
                  Map.of(
                      "label",
                      new NidiAttribute<>(
                          Label.html(
                              Util.truncate(
                                  "("
                                      + "?" // rule.getClass().getSimpleName().toLowerCase()
                                      + ") <br />"
                                      + lel
                                      // .replace("|", "_")
                                      // .replace("[", "_")
                                      // .replace("]", "_")
                                      // .replace("=", "_")
                                      // .replace("Σ", "_")
                                      // .replace("≤", "_")
                                      // .replace("≥", "_")
                                      + " ",
                                  16000)))));
            });

    ruleResult.collectInto(accumulatedConstraints);
  }

  public void plot() throws IOException {
    if (basePath == null) {
      return;
    }

    final var target = basePath.resolve(name + "-proof.svg");
    log.info(target.toString());

    final NidiExporter<Obligation, DefaultEdge> exporter = new NidiExporter<>(Util::stamp);
    exporter.setVertexAttributeProvider(vertexAttributes::get);
    exporter.setEdgeAttributeProvider(edgeAttributes::get);
    exporter.setGraphAttributeProvider(
        () -> Map.of("rankdir", new DefaultAttribute<>("BT", AttributeType.STRING)));

    Graphviz.fromGraph(exporter.transform(proof))
        .render(Format.SVG)
        .toOutputStream(Files.newOutputStream(target));
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve() {
    return ConstraintSystemSolver.solve(accumulatedConstraints, name);
  }
}
