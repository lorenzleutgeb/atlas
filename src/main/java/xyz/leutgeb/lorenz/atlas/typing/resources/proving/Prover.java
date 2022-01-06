package xyz.leutgeb.lorenz.atlas.typing.resources.proving;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toUnmodifiableMap;
import static xyz.leutgeb.lorenz.atlas.ast.Identifier.LEAF_NAME;
import static xyz.leutgeb.lorenz.atlas.util.Util.bug;
import static xyz.leutgeb.lorenz.atlas.util.Util.flag;
import static xyz.leutgeb.lorenz.atlas.util.Util.output;
import static xyz.leutgeb.lorenz.atlas.util.Util.stack;
import static xyz.leutgeb.lorenz.atlas.util.Util.supply;
import static xyz.leutgeb.lorenz.atlas.util.Util.undefinedText;
import static xyz.leutgeb.lorenz.atlas.util.Z3Support.load;

import com.google.common.collect.Sets;
import com.google.common.collect.Streams;
import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.GraphvizCmdLineEngine;
import java.io.IOException;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;
import lombok.Value;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.atlas.antlr.TacticLexer;
import xyz.leutgeb.lorenz.atlas.antlr.TacticParser;
import xyz.leutgeb.lorenz.atlas.ast.BooleanExpression;
import xyz.leutgeb.lorenz.atlas.ast.CallExpression;
import xyz.leutgeb.lorenz.atlas.ast.Expression;
import xyz.leutgeb.lorenz.atlas.ast.Identifier;
import xyz.leutgeb.lorenz.atlas.ast.IfThenElseExpression;
import xyz.leutgeb.lorenz.atlas.ast.LetExpression;
import xyz.leutgeb.lorenz.atlas.ast.MatchExpression;
import xyz.leutgeb.lorenz.atlas.ast.NodeExpression;
import xyz.leutgeb.lorenz.atlas.ast.ShareExpression;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.TacticVisitorImpl;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.rules.*;
import xyz.leutgeb.lorenz.atlas.typing.resources.rules.Rule.ApplicationResult;
import xyz.leutgeb.lorenz.atlas.typing.resources.solving.ConstraintSystemSolver;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.util.NidiAttribute;
import xyz.leutgeb.lorenz.atlas.util.NidiExporter;
import xyz.leutgeb.lorenz.atlas.util.Pair;
import xyz.leutgeb.lorenz.atlas.util.Util;

@Slf4j
public class Prover {
  private static final Rule letTreeCfPaperRule = LetTreeCf.INSTANCE;
  private static final Rule letTreeCfFlorianRule = LetTreeCfFlorian.INSTANCE;
  private static final Rule letTreeCfSimpleRule = LetTreeCfSimple.INSTANCE;
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
  private static final Rule shiftRule = Shift.INSTANCE;
  private static final Rule tickRule = Tick.INSTANCE;
  public static final Rule LET_TREE_CF = letTreeCfPaperRule;

  private static final Map<String, Rule> RULES_BY_NAME =
      Stream.of(
              LET_TREE_CF,
              letTreeCfFlorianRule,
              letGenRule,
              letTreeRule,
              applicationRule,
              comparisonRule,
              ifThenElseRule,
              matchRule,
              nodeRule,
              shareRule,
              variableRule,
              weakenVariableRule,
              weakenRule,
              leafRule,
              shiftRule,
              tickRule)
          .collect(toUnmodifiableMap(Rule::getName, identity()));

  private static final boolean DEFAULT_WEAKEN_AGGRESSIVELY = false;
  private static final boolean DEFAULT_WEAKEN_BEFORE_TERMINAL = true;

  private final String name;
  private final Path basePath;

  @Getter
  private final DirectedAcyclicGraph<Obligation, DefaultEdge> proof =
      new DirectedAcyclicGraph<>(DefaultEdge.class);

  private final Map<DefaultEdge, Map<String, Attribute>> edgeAttributes = new HashMap<>();

  @Getter private final Set<Constraint> accumulatedConstraints = new HashSet<>();

  // TODO(lorenz.leutgeb): Find a better way to handle globals...
  @Getter @Setter private AnnotatingGlobals globals;

  @Getter @Setter private boolean weakenAggressively = DEFAULT_WEAKEN_AGGRESSIVELY;
  @Getter @Setter private boolean weakenBeforeTerminal = DEFAULT_WEAKEN_BEFORE_TERMINAL;

  @Getter private final Map<String, Obligation> named = new LinkedHashMap<>();

  private final Map<Obligation, RuleApplication> results = new LinkedHashMap<>();

  @Getter @Setter private boolean logApplications = true;

  private final Set<Constraint> externalConstraints = new HashSet<>();

  @Getter @Setter private boolean weakenVariables = true;

  private final boolean naive;

  @Value
  @AllArgsConstructor
  private static class RuleApplication {
    Rule rule;
    ApplicationResult result;
  }

  @Value
  @AllArgsConstructor(access = AccessLevel.PRIVATE)
  private static class RuleSchedule {
    Rule rule;
    Map<String, String> arguments;

    public static RuleSchedule schedule(Rule rule) {
      return new RuleSchedule(rule, emptyMap());
    }

    public static RuleSchedule schedule(Rule rule, Map<String, String> arguments) {
      return new RuleSchedule(rule, arguments);
    }
  }

  public Prover(String name, AnnotatingGlobals globals, Path basePath) {
    load(basePath.resolve("z3.log"));
    this.naive = flag(Prover.class, emptyMap(), "naive");
    this.name = name;
    this.globals = globals;
    this.basePath = basePath;
  }

  /** Chooses which rules should be applied (in order) to prove given obligation. */
  private Stack<RuleSchedule> chooseRules(Obligation obligation) {
    final var expression = obligation.getExpression();
    if (expression.isTerminal()) {
      final Stack<RuleSchedule> todo = new Stack<>();
      todo.push(RuleSchedule.schedule(chooseRule(expression)));
      if (expression instanceof CallExpression) {
        log.trace("Automatically applying (tick) to expression `{}`!", expression);
        todo.push(RuleSchedule.schedule(tickRule));
      }
      if (weakenBeforeTerminal || weakenAggressively) {
        todo.push(RuleSchedule.schedule(weakenRule));
      }
      final var wvars = WVar.redundantIds(obligation).collect(Collectors.toUnmodifiableList());
      for (int i = 0; i < wvars.size(); i++) {
        todo.push(RuleSchedule.schedule(weakenVariableRule));
      }
      if (wvars.size() > 0) {
        todo.push(RuleSchedule.schedule(weakenRule));
      }
      return todo;
    } else if (weakenAggressively) {
      log.trace(
          "Automatically applying (w) to expression `{}` because aggressive weakening is enabled!",
          expression);
      return stack(
          RuleSchedule.schedule(weakenRule), RuleSchedule.schedule(chooseRule(expression)));
    } else {
      return stack(RuleSchedule.schedule(chooseRule(expression)));
    }
  }

  /** Chooses which rule should be applied next to given expression. */
  private Rule chooseRule(Expression e) {
    if (e instanceof BooleanExpression) {
      return comparisonRule;
    } else if (e instanceof NodeExpression) {
      return nodeRule;
    } else if (e instanceof CallExpression) {
      return applicationRule;
    } else if (e instanceof Identifier) {
      if (LEAF_NAME.equals(((Identifier) e).getName())) {
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
        // TODO(lorenz.leutgeb): When can we apply let:tree?
        return LET_TREE_CF;
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

  private Stack<RuleSchedule> chooseRuleSmart(Obligation obligation) {
    final var e = obligation.getExpression();
    if (e instanceof CallExpression) {
      final var isInLet =
          obligation
              .getParent()
              .map(parent -> parent.getExpression() instanceof LetExpression)
              .orElse(false);

      final Stack<RuleSchedule> todo = new Stack<>();
      todo.push(RuleSchedule.schedule(applicationRule));
      todo.push(RuleSchedule.schedule(shiftRule));
      log.trace("Automatically applying (tick) to expression `{}`!", e);
      todo.push(RuleSchedule.schedule(tickRule));
      weakenVariables(obligation, todo);
      if (!isInLet) {
        todo.push(RuleSchedule.schedule(weakenRule, Map.of("l2xy", "true")));
      }
      return todo;
    } else if (e instanceof Identifier) {
      final var isLeaf = ((Identifier) e).getName().equals("leaf");
      final Stack<RuleSchedule> todo = new Stack<>();
      todo.push(RuleSchedule.schedule(isLeaf ? leafRule : variableRule));
      weakenVariables(obligation, todo);
      todo.push(RuleSchedule.schedule(weakenRule, Map.of("mono", "true")));
      return todo;
    } else if (e instanceof NodeExpression) {
      final var lastInChain =
          obligation
              .getParent()
              .map(
                  parent ->
                      parent.getExpression() instanceof LetExpression
                          && ((LetExpression) parent.getExpression()).getBody().equals(e))
              .orElse(false);
      // Covers outermost node of a tree construction.
      final Stack<RuleSchedule> todo = new Stack<>();
      todo.push(RuleSchedule.schedule(nodeRule));
      weakenVariables(obligation, todo);
      todo.push(
          RuleSchedule.schedule(
              weakenRule, lastInChain ? Map.of("l2xy", "true") : Map.of("mono", "true")));
      return todo;
    } else if (e instanceof LetExpression) {
      final var let = (LetExpression) e;
      if (let.getValue() instanceof CallExpression) {
        // Makes sure we apply (w{l2xy}) right before (app).
        return stack(
            RuleSchedule.schedule(weakenRule, Map.of("l2xy", "true", "mono", "true")),
            RuleSchedule.schedule(LET_TREE_CF));
      } else if (let.isTreeConstruction()) {
        if (let.getValue() instanceof NodeExpression) {
          // Makes sure we apply (w{size}) if we're constructing a tree.
          return stack(
              RuleSchedule.schedule(weakenRule, Map.of("size", "true")),
              RuleSchedule.schedule(LET_TREE_CF));
        }
      }
    }
    return stack(RuleSchedule.schedule(chooseRule(e)));
  }

  private ApplicationResult applyInternal(Obligation obligation, RuleSchedule schedule) {
    if (logApplications && obligation.isCost()) {
      log.info(
          "{}{}: {}",
          String.format("%-12s", schedule.rule.getName()),
          schedule.arguments,
          obligation.getExpression());
    }

    final var result = schedule.rule.apply(obligation, globals, schedule.arguments);
    if (result == null) {
      throw bug("typing rule implementation returned null");
    }
    ingest(obligation, schedule.rule, result);
    return result;
  }

  public List<Obligation> apply(Obligation obligation, Rule rule) {
    return apply(obligation, rule, emptyMap());
  }

  public List<Obligation> apply(
      Obligation obligation, Rule rule, Map<String, String> ruleArguments) {
    return applyInternal(obligation, RuleSchedule.schedule(rule, ruleArguments)).obligations();
  }

  public void prove(Obligation obligation) {
    if (naive) {
      this.setWeakenAggressively(true);
      this.proveInternal(obligation);
      this.setWeakenAggressively(false);
      return;
    }
    this.proveInternal(obligation);
  }

  /** Translates given obligation into a set of constraints that would prove given obligation. */
  public void proveInternal(Obligation obligation) {
    Stack<Obligation> obligations = new Stack<>();
    obligations.push(obligation);
    proof.addVertex(obligation);

    while (!obligations.isEmpty()) {
      final var top = obligations.pop();
      final var schedule = naive ? chooseRules(top) : chooseRuleSmart(top);

      var nextObligation = top;
      ApplicationResult ruleResult = null;

      while (!schedule.isEmpty()) {
        ruleResult = applyInternal(nextObligation, schedule.pop());

        if (!schedule.isEmpty()) {
          if (ruleResult.obligations().size() > 1) {
            throw bug(
                "if there are multiple rule applications scheduled, all of them except the last must return exactly one obligation");
          }

          if (ruleResult.obligations().isEmpty()) {
            throw bug("multiple rule applications were scheduled but we ran out of obligations");
          }

          nextObligation = ruleResult.obligations().get(0);
        } else {
          obligations.addAll(ruleResult.obligations());
        }
      }
    }
  }

  private void ingest(Obligation previous, Rule rule, ApplicationResult ruleResult) {
    results.put(previous, new RuleApplication(rule, ruleResult));

    if (!proof.containsVertex(previous)) {
      proof.addVertex(previous);
    }

    ruleResult
        .obligations()
        .forEach(
            o -> {
              if (!proof.containsVertex(o)) {
                proof.addVertex(o);
              }
            });

    if (ruleResult.obligations().size() == 1 && previous == ruleResult.obligations().get(0)) {
      log.warn(
          "Detected a noop generated by rule "
              + rule.getName()
              + " for an obligation with"
              + (previous.isCost() ? "" : "out")
              + " cost ");
      return;
    }

    Streams.zip(ruleResult.obligations().stream(), ruleResult.constraints().stream(), Pair::of)
        .forEach(
            o -> {
              final var edge = proof.addEdge(previous, o.getLeft());
              edgeAttributes.put(
                  edge,
                  Map.of(
                      "label",
                      new NidiAttribute<>(
                          Label.html(
                              Util.truncate(
                                  "("
                                      + rule.getName()
                                      + ")"
                                      + "<br />"
                                      + (!rule.equals(weakenRule) || o.getRight().size() <= 32
                                          ? (o.getRight().isEmpty()
                                              ? "No constraints."
                                              : (o.getRight().stream()
                                                  .map(Constraint::toStringWithReason)
                                                  .collect(Collectors.joining("<br />"))))
                                          : "Constraints omitted.")
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

  public void plot() {
    if (basePath == null) {
      log.warn("Cannot plot without base path.");
      return;
    }

    try {
      final NidiExporter<Obligation, DefaultEdge> exporter = new NidiExporter<>(Util::stamp);
      exporter.setVertexAttributeProvider(
          obligation -> {
            var result = results.get(obligation);
            final List<Constraint> generalConstraints =
                result != null ? result.result.generalConstraints() : emptyList();
            return obligation.attributes(generalConstraints);
          });
      exporter.setEdgeAttributeProvider(edgeAttributes::get);
      exporter.setGraphAttributeProvider(
          supply(Map.of("rankdir", new DefaultAttribute<>("BT", AttributeType.STRING))));

      Graphviz transformed = Graphviz.fromGraph(exporter.transform(proof));

      final var target = basePath.resolve(name + "-proof.svg");
      transformed.render(Format.SVG).toOutputStream(output(target));
      log.info("Proof plotted to {}", target);

      final var dotTarget = basePath.resolve(name + "-proof.dot");
      transformed.render(Format.DOT).toOutputStream(output(dotTarget));
      log.info("Proof exported to {}", dotTarget);
    } catch (Throwable e) {
      log.warn("Non-critical exception thrown.", e);
    }
  }

  public void plotWithSolution(Map<Coefficient, KnownCoefficient> solution) {
    if (basePath == null) {
      return;
    }
    try {
      final NidiExporter<Obligation, DefaultEdge> exporter = new NidiExporter<>(Util::stamp);
      exporter.setVertexAttributeProvider(
          obligation -> {
            var result = results.get(obligation);
            final List<Constraint> generalConstraints =
                result != null ? result.result.generalConstraints() : emptyList();
            return (obligation.substitute(solution)).attributes(generalConstraints);
          });
      exporter.setEdgeAttributeProvider(edgeAttributes::get);
      exporter.setGraphAttributeProvider(
          () -> Map.of("rankdir", new DefaultAttribute<>("BT", AttributeType.STRING)));

      Graphviz transformed = Graphviz.fromGraph(exporter.transform(proof));

      Graphviz.useEngine(new GraphvizCmdLineEngine());

      final var dotTarget = basePath.resolve(name + "-proof.xdot");
      transformed.render(Format.XDOT).toOutputStream(output(dotTarget));
      log.info("Proof exported to {}", dotTarget);

      final var target = basePath.resolve(name + "-proof.svg");
      transformed.render(Format.SVG).toOutputStream(output(target));
      log.info("Proof plotted to {}", target);
    } catch (Exception e) {
      log.warn("Non-critical exception thrown.", e);
    }
  }

  public ConstraintSystemSolver.Result solve() {
    return solve(emptySet(), emptyList());
  }

  public ConstraintSystemSolver.Result solve(Set<Constraint> outsideConstraints) {
    return solve(outsideConstraints, emptyList());
  }

  public ConstraintSystemSolver.Result solve(
      Set<Constraint> outsideConstraints, List<UnknownCoefficient> target) {
    return ConstraintSystemSolver.solve(
        Sets.union(outsideConstraints, Sets.union(accumulatedConstraints, externalConstraints)),
        basePath.resolve(name),
        target);
  }

  public ConstraintSystemSolver.Result solve(
      Set<Constraint> outsideConstraints, List<UnknownCoefficient> target, String suffix) {
    return ConstraintSystemSolver.solve(
        Sets.union(outsideConstraints, Sets.union(accumulatedConstraints, externalConstraints)),
        basePath.resolve(suffix),
        target);
  }

  public Obligation weaken(Obligation obligation) {
    final var result = apply(obligation, weakenRule);
    if (result.size() != 1) {
      throw bug("(w) did not produce exactly one new obligation!");
    }
    return result.get(0);
  }

  public List<Obligation> applyByName(
      String ruleName, Map<String, String> ruleArguments, Obligation obligation) {
    Rule rule = RULES_BY_NAME.get(ruleName);
    if (rule == null) {
      throw new IllegalArgumentException(
          "Rule name " + undefinedText(ruleName, RULES_BY_NAME.keySet()));
    }
    return apply(obligation, rule, ruleArguments);
  }

  public void record(String name, Obligation obligation) {
    if (named.containsKey(name)) {
      throw new IllegalArgumentException("name already taken");
    }
    this.named.put(name, obligation);
  }

  public void weakenVariables(Obligation obligation, Stack<RuleSchedule> todo) {
    final var wvars = WVar.redundantIds(obligation).collect(Collectors.toUnmodifiableList());
    for (int i = 0; i < wvars.size(); i++) {
      todo.push(RuleSchedule.schedule(weakenVariableRule));
    }
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

  public void addExternalConstraints(Collection<Constraint> external) {
    this.externalConstraints.addAll(external);
  }
}
