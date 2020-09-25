package xyz.leutgeb.lorenz.lac.typing.resources.proving;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptySet;
import static java.util.Collections.singletonList;
import static xyz.leutgeb.lorenz.lac.ast.Identifier.LEAF;
import static xyz.leutgeb.lorenz.lac.util.Util.bug;
import static xyz.leutgeb.lorenz.lac.util.Util.stack;

import com.google.common.collect.Sets;
import com.google.common.collect.Streams;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.Stack;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.lac.antlr.TacticLexer;
import xyz.leutgeb.lorenz.lac.antlr.TacticParser;
import xyz.leutgeb.lorenz.lac.ast.BooleanExpression;
import xyz.leutgeb.lorenz.lac.ast.CallExpression;
import xyz.leutgeb.lorenz.lac.ast.Expression;
import xyz.leutgeb.lorenz.lac.ast.Identifier;
import xyz.leutgeb.lorenz.lac.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.lac.ast.LetExpression;
import xyz.leutgeb.lorenz.lac.ast.MatchExpression;
import xyz.leutgeb.lorenz.lac.ast.ShareExpression;
import xyz.leutgeb.lorenz.lac.ast.Tuple;
import xyz.leutgeb.lorenz.lac.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.lac.typing.resources.TacticVisitorImpl;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.App;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Cmp;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Ite;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Leaf;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetGen;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTree;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.LetTreeCfSimple;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Match;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Node;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Rule;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Share;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Var;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.WVar;
import xyz.leutgeb.lorenz.lac.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.lac.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.lac.util.NidiAttribute;
import xyz.leutgeb.lorenz.lac.util.NidiExporter;
import xyz.leutgeb.lorenz.lac.util.Pair;
import xyz.leutgeb.lorenz.lac.util.Util;

@Slf4j
public class Prover {
  private static final Rule letTreeCfRule = LetTreeCfSimple.INSTANCE;
  private static final Rule letGenRule = LetGen.INSTANCE;
  private static final Rule letTreeRule = LetTree.INSTANCE;
  private static final Rule applicationRule = App.INSTANCE;
  private static final Rule comparisonRule = Cmp.INSTANCE;
  private static final Rule ifThenElseRule = Ite.INSTANCE;
  private static final Rule matchRule = Match.INSTANCE;
  private static final Rule nodeRule = Node.INSTANCE;
  private static final Rule shareRule = Share.INSTANCE;
  private static final Rule variableRule = Var.INSTANCE;
  private static final Rule weakenVariableRule = WVar.INSTANCE;
  private static final Rule weakenRule = W.INSTANCE;
  private static final Rule leafRule = Leaf.INSTANCE;

  private static final Map<String, Rule> RULES_BY_NAME =
      Map.<String, Rule>of(
          "let:tree:cf",
          letTreeCfRule,
          letGenRule.getName(),
          letGenRule,
          letTreeRule.getName(),
          letTreeRule,
          applicationRule.getName(),
          applicationRule,
          comparisonRule.getName(),
          comparisonRule,
          ifThenElseRule.getName(),
          ifThenElseRule,
          matchRule.getName(),
          matchRule,
          nodeRule.getName(),
          nodeRule,
          shareRule.getName(),
          shareRule,
          weakenRule.getName(),
          weakenRule);

  private static final boolean DEFAULT_WEAKEN = false;
  private static final boolean DEFAULT_TREECF = false;
  private static final boolean DEFAULT_AUTO = false;

  private final String name;
  private final Path basePath;

  @Getter
  private final DirectedAcyclicGraph<Obligation, DefaultEdge> proof =
      new DirectedAcyclicGraph<>(DefaultEdge.class);

  private final Map<DefaultEdge, Map<String, Attribute>> edgeAttributes = new HashMap<>();
  private final Map<Obligation, Map<String, Attribute>> vertexAttributes = new HashMap<>();

  @Getter private final Set<Constraint> accumulatedConstraints = new HashSet<>();

  // TODO(lorenz.leutgeb): Find a better way to handle globals...
  @Getter @Setter private AnnotatingGlobals globals;

  @Getter @Setter private boolean weakenBeforeTerminal = DEFAULT_WEAKEN;

  @Getter @Setter private boolean auto = DEFAULT_AUTO;

  @Getter @Setter private boolean treeCf = DEFAULT_TREECF;

  @Getter private final Map<String, Obligation> named = new LinkedHashMap<String, Obligation>();

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
    // if (obligation.isNothing()) {
    //  return stack();
    // }

    // TODO(lorenz.leutgeb): Be smart about the application of (w:var) and (w). (w) should probably
    // be applied
    // maximizing the knowledge about identifiers in the context. This means that (w:var) should
    // not be applied in some cases since it removes identifiers from the context that might carry
    // otherwise useful information?!

    final var expression = obligation.getExpression();
    if (WVar.redundantId(obligation).isPresent()) {
      return stack(weakenVariableRule);
    }
    if (expression.isTerminal()) {
      if (weakenBeforeTerminal || auto) {
        /*
        log.info(
            "Scheduling (w) for application to terminal expression {} because weakening is enabled!",
            expression);
         */
        return stack(weakenRule, chooseRule(expression));
      } else {
        /*
        log.info(
            "Not scheduling (w) for application to terminal expression {} because weakening is disabled.",
            expression);
         */
        return stack(chooseRule(expression));
      }
    } else {
      if (!auto || (expression instanceof IfThenElseExpression)) {
        return stack(chooseRule(expression));
      } else {
        return stack(weakenRule, chooseRule(expression));
      }
    }
  }

  /** Chooses which rule should be applied next to given expression. */
  private Rule chooseRule(Expression e) {
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
        // TODO(lorenz.leutgeb): When should we apply the cost-free version?
        if (treeCf || auto) {
          return letTreeCfRule;
        } else {
          return letTreeRule;
        }
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
      final var result = applyInternal(obligation, shareRule);
      obligation = result.getObligations().get(0);
    }
    return obligation;
  }

  public List<Obligation> proveUntilExpressionEquals(Obligation obligation, Expression expression) {
    return proveUntil(obligation, x -> expression.equals(x.getExpression()));
  }

  public List<Obligation> proveUntil(
      List<Obligation> obligations, Predicate<Obligation> condition) {
    return obligations.stream()
        .flatMap(o -> proveUntil(o, condition).stream())
        .collect(Collectors.toList());
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
      final var ruleResult = applyInternal(obligation, rule);
      return ruleResult.getObligations().stream()
          .flatMap(x -> proveUntil(x, condition).stream())
          .collect(Collectors.toList());
    }

    Rule.ApplicationResult ruleResult = null;
    while (!rules.isEmpty()) {
      final Rule rule = rules.pop();
      ruleResult = applyInternal(obligation, rule);
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

  private Rule.ApplicationResult applyInternal(Obligation obligation, Rule rule) {
    log.debug("{}: {}", String.format("%-12s", rule.getName()), obligation);
    final var result = rule.apply(obligation, globals);
    if (result == null) {
      throw bug("typing rule implementation returned null");
    }
    ingest(obligation, rule, result);
    return result;
  }

  public List<Obligation> apply(Obligation obligation, Rule rule) {
    return applyInternal(obligation, rule).getObligations();
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
        ruleResult = applyInternal(nextObligation, rule);

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

  private void ingest(Obligation previous, Rule rule, Rule.ApplicationResult ruleResult) {
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

    final var generalConstraints = ruleResult.getGeneralConstraints();

    Streams.zip(
            ruleResult.getObligations().stream(), ruleResult.getConstraints().stream(), Pair::of)
        .forEach(
            o -> {
              final var edge = proof.addEdge(previous, o.getLeft());
              final var lel =
                  o.getRight().stream().map(Object::toString).collect(Collectors.joining("<br />"))
                      + (generalConstraints.isEmpty()
                          ? ""
                          : "<br /> - - - - <br />"
                              + generalConstraints.stream()
                                  .map(Object::toString)
                                  .collect(Collectors.joining("<br />")));
              edgeAttributes.put(
                  edge,
                  Map.of(
                      "label",
                      new NidiAttribute<>(
                          Label.html(
                              Util.truncate(
                                  rule.getName()
                                      + "<br />"
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

    log.info("Proof plotted to {}", target);
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve() {
    return solve(emptySet(), emptyList());
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(Set<Constraint> outsideConstraints) {
    return solve(outsideConstraints, emptyList());
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Set<Constraint> outsideConstraints, List<Coefficient> target) {
    return ConstraintSystemSolver.solve(
        Sets.union(outsideConstraints, accumulatedConstraints), name, target);
  }

  public Optional<Map<Coefficient, KnownCoefficient>> solve(
      Set<Constraint> outsideConstraints,
      List<Coefficient> target,
      ConstraintSystemSolver.Domain domain) {
    return ConstraintSystemSolver.solve(
        Sets.union(outsideConstraints, accumulatedConstraints), name, target, domain);
  }

  public Obligation weaken(Obligation obligation) {
    final var result = apply(obligation, weakenRule);
    if (result.size() != 1) {
      throw bug("(w) did not produce exactly one new obligation!");
    }
    return result.get(0);
  }

  public List<Obligation> applyByName(String ruleName, Obligation obligation) {
    return apply(obligation, RULES_BY_NAME.get(ruleName));
  }

  public void record(String name, Obligation obligation) {
    if (named.containsKey(name)) {
      throw new IllegalArgumentException("name already taken");
    }
    this.named.put(name, obligation);
  }

  public Obligation weakenVariables(Obligation obligation) {
    if (WVar.redundantId(obligation).isPresent()) {
      return weakenVariables(apply(obligation, weakenVariableRule).get(0));
    }
    return obligation;
  }

  public void read(Obligation root, Path path) throws IOException {
    final var visitor = new TacticVisitorImpl(this, root);
    visitor.visitTactic(
        new TacticParser(new CommonTokenStream(new TacticLexer(CharStreams.fromPath(path))))
            .tactic());
  }
}
