package xyz.leutgeb.lorenz.atlas.typing.resources.proving;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toUnmodifiableMap;
import static xyz.leutgeb.lorenz.atlas.ast.Identifier.LEAF_NAME;
import static xyz.leutgeb.lorenz.atlas.util.Util.*;
import static xyz.leutgeb.lorenz.atlas.util.Z3Support.load;

import com.google.common.collect.Sets;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.GraphvizCmdLineEngine;
import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.jgrapht.graph.AsSubgraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.atlas.antlr.TacticLexer;
import xyz.leutgeb.lorenz.atlas.antlr.TacticParser;
import xyz.leutgeb.lorenz.atlas.ast.*;
import xyz.leutgeb.lorenz.atlas.ast.sources.Derived;
import xyz.leutgeb.lorenz.atlas.typing.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.atlas.typing.resources.Tactic;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.Coefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.KnownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.coefficients.UnknownCoefficient;
import xyz.leutgeb.lorenz.atlas.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.atlas.typing.resources.rules.*;
import xyz.leutgeb.lorenz.atlas.typing.resources.rules.Rule.ApplicationResult;
import xyz.leutgeb.lorenz.atlas.typing.resources.solving.Solver;
import xyz.leutgeb.lorenz.atlas.typing.simple.types.TreeType;
import xyz.leutgeb.lorenz.atlas.util.NidiExporter;
import xyz.leutgeb.lorenz.atlas.util.Util;

@Slf4j
public class Prover {
  private static final Rule RULE_LET_TREE_CF = LetTreeCf.INSTANCE;
  // private static final Rule RULE_LET_TREE_CF = LetTreeCfFlorian.INSTANCE;
  private static final Rule RULE_APP = App.INSTANCE;
  private static final Rule RULE_CMP = Cmp.INSTANCE;
  private static final Rule RULE_ITE = Ite.INSTANCE;
  private static final Rule RULE_MATCH = Match.INSTANCE;
  private static final Rule RULE_NODE = Node.INSTANCE;
  private static final Rule RULE_SHARE = Share.INSTANCE;
  private static final Rule RULE_VAR = Var.INSTANCE;
  private static final Rule RULE_W_VAR = WVar.INSTANCE;
  private static final Rule RULE_W = W.INSTANCE;
  private static final Rule RULE_LEAF = Leaf.INSTANCE;
  private static final Rule RULE_SHIFT = Shift.INSTANCE;
  private static final Rule RULE_TICK = Tick.INSTANCE;

  private static final Map<String, Rule> RULES_BY_NAME =
      Stream.of(
              RULE_LET_TREE_CF,
              RULE_APP,
              RULE_CMP,
              RULE_ITE,
              RULE_MATCH,
              RULE_NODE,
              RULE_SHARE,
              RULE_VAR,
              RULE_W_VAR,
              RULE_W,
              RULE_LEAF,
              RULE_SHIFT,
              RULE_TICK)
          .collect(toUnmodifiableMap(Rule::getName, identity()));

  private static final boolean DEFAULT_WEAKEN_AGGRESSIVELY = false;
  private static final boolean DEFAULT_WEAKEN_BEFORE_TERMINAL = true;

  private static final boolean TICK_BEFORE_APP = false;

  private final String name;
  private final Path basePath;

  public record ProofVertexData(
      RuleSchedule schedule,
      List<List<Constraint>> constraints,
      List<Constraint> generalConstraints) {}

  @Getter
  private final DirectedAcyclicGraph<Obligation, DefaultEdge> proof =
      new DirectedAcyclicGraph<>(DefaultEdge.class);

  private final Map<Obligation, ProofVertexData> vertexAttributes = new HashMap<>();

  @Getter private final Set<Constraint> accumulatedConstraints = new HashSet<>();

  // TODO(lorenzleutgeb): Find a better way to handle globals...
  @Getter @Setter private AnnotatingGlobals globals;

  @Getter @Setter private boolean weakenAggressively = DEFAULT_WEAKEN_AGGRESSIVELY;
  @Getter @Setter private boolean weakenBeforeTerminal = DEFAULT_WEAKEN_BEFORE_TERMINAL;

  @Getter private final Map<String, Obligation> named = new LinkedHashMap<>();

  @Getter @Setter private boolean logApplications = true;

  private final Set<Constraint> externalConstraints = new HashSet<>();

  @Getter @Setter private boolean weakenVariables = true;

  private final boolean naive;

  public record RuleSchedule(Rule rule, Map<String, String> arguments) {
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
      if (expression instanceof CallExpression && TICK_BEFORE_APP) {
        log.trace("Automatically applying (tick) to expression `{}`!", expression);
        todo.push(RuleSchedule.schedule(RULE_TICK));
      }
      if (weakenBeforeTerminal || weakenAggressively) {
        todo.push(RuleSchedule.schedule(RULE_W));
      }
      int beforeWeakeningVars = todo.size();
      scheduleWeakenVariables(obligation, todo);
      if (todo.size() > beforeWeakeningVars) {
        todo.push(RuleSchedule.schedule(RULE_W));
      }
      return todo;
    } else if (weakenAggressively) {
      log.trace(
          "Automatically applying (w) to expression `{}` because aggressive weakening is enabled!",
          expression);
      return stack(RuleSchedule.schedule(RULE_W), RuleSchedule.schedule(chooseRule(expression)));
    } else {
      return stack(RuleSchedule.schedule(chooseRule(expression)));
    }
  }

  /** Chooses which rule should be applied next to given expression. */
  private Rule chooseRule(Expression e) {
    if (e instanceof BooleanExpression) {
      return RULE_CMP;
    } else if (e instanceof NodeExpression) {
      return RULE_NODE;
    } else if (e instanceof CallExpression) {
      return RULE_APP;
    } else if (e instanceof Identifier identifier) {
      return LEAF_NAME.equals(identifier.getName()) ? RULE_LEAF : RULE_VAR;
    } else if (e instanceof IfThenElseExpression) {
      return RULE_ITE;
    } else if (e instanceof MatchExpression) {
      return RULE_MATCH;
    } else if (e instanceof LetExpression letExpression) {
      if (letExpression.getValue().getType() instanceof TreeType) {
        // TODO(lorenzleutgeb): When can we apply let:tree?
        return RULE_LET_TREE_CF;
      }
    } else if (e instanceof ShareExpression) {
      return RULE_SHARE;
    } else if (e instanceof TickExpression) {
      return RULE_TICK;
    }
    throw bug("could not choose a rule for expression of type " + e.getClass().getCanonicalName());
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
      todo.push(RuleSchedule.schedule(RULE_APP));
      todo.push(RuleSchedule.schedule(RULE_SHIFT));
      if (TICK_BEFORE_APP) {
        log.trace("Automatically applying (tick) to expression `{}`!", e);
        todo.push(RuleSchedule.schedule(RULE_TICK));
      }
      scheduleWeakenVariables(obligation, todo);
      if (!isInLet) {
        todo.push(RuleSchedule.schedule(RULE_W, Map.of("l2xy", "true")));
      }
      return todo;
    } else if (e instanceof final Identifier identifier) {
      final var isLeaf = identifier.getName().equals("leaf");
      final Stack<RuleSchedule> todo = new Stack<>();
      todo.push(RuleSchedule.schedule(isLeaf ? RULE_LEAF : RULE_VAR));
      scheduleWeakenVariables(obligation, todo);
      todo.push(RuleSchedule.schedule(RULE_W, Map.of("mono", "true")));
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
      todo.push(RuleSchedule.schedule(RULE_NODE));
      scheduleWeakenVariables(obligation, todo);
      todo.push(
          RuleSchedule.schedule(
              RULE_W, lastInChain ? Map.of("l2xy", "true") : Map.of("mono", "true")));
      return todo;
    } else if (e instanceof final LetExpression letExpression) {
      final var value = letExpression.getValue();
      if (value instanceof CallExpression || value instanceof TickExpression) {
        // Makes sure we apply (w{l2xy}) right before (app) or (tick).
        return stack(
            RuleSchedule.schedule(RULE_W, Map.of("l2xy", "true", "mono", "true")),
            RuleSchedule.schedule(RULE_LET_TREE_CF));
      } else if (letExpression.isTreeConstruction()) {
        if (letExpression.getValue() instanceof NodeExpression) {
          // Makes sure we apply (w{size}) if we're constructing a tree.
          return stack(
              RuleSchedule.schedule(RULE_W, Map.of("size", "true")),
              RuleSchedule.schedule(RULE_LET_TREE_CF));
        }
      }
    } else if (e instanceof TickExpression) {
      return stack(
          RuleSchedule.schedule(RULE_W, Map.of("l2xy", "true")), RuleSchedule.schedule(RULE_TICK));
    }
    return stack(RuleSchedule.schedule(chooseRule(e)));
  }

  private ApplicationResult applyInternal(Obligation obligation, RuleSchedule schedule) {
    if (logApplications && obligation.isCost()) {
      log.trace(
          "{}{}: {}",
          String.format("%-12s", schedule.rule.getName()),
          schedule.arguments,
          obligation.getExpression());
    }

    final var result = schedule.rule.apply(obligation, globals, schedule.arguments);
    if (result == null) {
      throw bug("typing rule implementation returned null");
    }
    ingest(obligation, schedule, result);
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

  private void ingest(Obligation previous, RuleSchedule schedule, ApplicationResult ruleResult) {
    if (!proof.containsVertex(previous)) {
      proof.addVertex(previous);
    }

    if (vertexAttributes.containsKey(previous)) {
      throw bug("one obligation is reached from multiple places");
    }
    vertexAttributes.put(
        previous,
        new ProofVertexData(schedule, ruleResult.constraints(), ruleResult.generalConstraints()));

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
              + schedule.rule.getName()
              + " for an obligation with"
              + (previous.isCost() ? "" : "out")
              + " cost ");
      return;
    }

    for (Obligation obligation : ruleResult.obligations()) {
      proof.addEdge(previous, obligation);
    }

    ruleResult.collectInto(accumulatedConstraints);
  }

  public void printTactic(Obligation root, PrintStream out, boolean withCostOnly) {

    if (!proof.containsVertex(root)) {
      // throw new IllegalArgumentException("unknown obligation");
      return;
    }

    metaToTactic(root, withCostOnly, out, 0);

    out.println();
  }

  private void metaToTactic(
      Obligation obligation, boolean costOnly, PrintStream out, int indentation) {
    if (costOnly && !obligation.isCost()) {
      return;
    }
    final var meta = vertexAttributes.get(obligation);
    if (meta == null) {
      return;
    }
    final var args = meta.schedule().arguments();
    final var leaf = meta.constraints.isEmpty();
    final var manyDescendants = proof.getDescendants(obligation).size() > 4;
    final var compress = !manyDescendants;
    final var rule = meta.schedule().rule();
    // final var skip = rule instanceof WVar;

    indent(out, indentation);
    if (!leaf) {
      out.print("(");
    }

    out.print(rule.getName());

    if (!args.isEmpty()) {
      out.print(
          args.entrySet().stream()
              .map(Object::toString)
              .map(s -> s.replaceAll("=true$", ""))
              .collect(Collectors.joining(" ", "{", "}")));
    }

    if (obligation.getExpression() instanceof IfThenElseExpression ifThenElseExpression) {
      if (ifThenElseExpression.getCondition().isTerminal()) {
        out.print(" (* " + ifThenElseExpression.getCondition() + " *)");
      }
    } else if (obligation.getExpression() instanceof MatchExpression matchExpression) {
      if (matchExpression.getScrut().getSource() instanceof Derived derived) {
        out.print(" (* " + derived.getParent() + " *)");
      } else if (matchExpression.getScrut().isTerminal()) {
        out.print(" (* " + matchExpression.getScrut() + " *)");
      }
    }

    if (!compress && !leaf) {
      out.println();
    }

    if (compress && !leaf) {
      out.print(" ");
    }

    final var outgoing = List.copyOf(proof.outgoingEdgesOf(obligation));
    for (int i = 0; i < outgoing.size(); i++) {
      final var edge = outgoing.get(i);
      metaToTactic(proof.getEdgeTarget(edge), costOnly, out, compress ? 0 : indentation + 1);
      if (compress && !leaf && i < outgoing.size() - 1) {
        out.print(" ");
      }
    }

    if (!leaf) {
      if (!compress) {
        indent(out, indentation);
      }
      out.print(")");
      // if (!compress) {
      if (indentation > 0) {
        out.println();
      }
      // }
    }
  }

  public void plot(Obligation root) {
    if (basePath == null) {
      log.warn("Cannot plot without base path.");
      return;
    }

    try {
      final NidiExporter<Obligation, DefaultEdge> exporter = new NidiExporter<>(Util::stamp);
      exporter.setVertexAttributeProvider(
          obligation -> obligation.attributes(vertexAttributes.get(obligation)));
      exporter.setGraphAttributeProvider(
          supply(Map.of("rankdir", new DefaultAttribute<>("BT", AttributeType.STRING))));

      Graphviz transformed = Graphviz.fromGraph(exporter.transform(proof));

      final var target = basePath.resolve("proof").resolve(name + ".svg");
      transformed.render(Format.SVG).toOutputStream(output(target));
      log.info("See {}", target);

      /*
      final var dotTarget = basePath.resolve(name + "-proof.dot");
      transformed.render(Format.DOT).toOutputStream(output(dotTarget));
      log.info("Proof exported to {}", dotTarget);
       */
    } catch (Throwable e) {
      log.warn("Non-critical exception thrown.", e);
    }
  }

  public void plotWithSolution(
      Map<Coefficient, KnownCoefficient> solution, Obligation root, boolean costOnly) {
    if (basePath == null) {
      return;
    }
    if (!proof.containsVertex(root)) {
      throw new IllegalArgumentException("unknown root obligation");
    }
    try {
      final var descendants = proof.getDescendants(root);
      final var filtered =
          costOnly
              ? new AsSubgraph<>(
                  proof,
                  costOnly
                      ? descendants.stream().filter(Obligation::isCost).collect(Collectors.toSet())
                      : descendants)
              : proof;

      final NidiExporter<Obligation, DefaultEdge> exporter = new NidiExporter<>(Util::stamp);
      exporter.setVertexAttributeProvider(
          obligation ->
              (obligation.substitute(solution)).attributes(vertexAttributes.get(obligation)));
      exporter.setGraphAttributeProvider(
          () -> Map.of("rankdir", new DefaultAttribute<>("BT", AttributeType.STRING)));

      Graphviz transformed = Graphviz.fromGraph(exporter.transform(filtered));

      Graphviz.useEngine(new GraphvizCmdLineEngine());

      /*
      final var dotTarget = basePath.resolve(name + "-proof.xdot");
      transformed.render(Format.XDOT).toOutputStream(output(dotTarget));
      log.info("Proof exported to {}", dotTarget);
       */

      final var target = basePath.resolve("proof").resolve(name + ".svg");
      transformed.render(Format.SVG).toOutputStream(output(target));
      log.info("See {}", target);
    } catch (Exception e) {
      log.warn("Non-critical exception thrown.", e);
    }
  }

  public Solver.Result solve() {
    return solve(emptySet(), emptyList());
  }

  public Solver.Result solve(Set<Constraint> outsideConstraints) {
    return solve(outsideConstraints, emptyList());
  }

  public Solver.Result solve(Set<Constraint> outsideConstraints, List<UnknownCoefficient> target) {
    return Solver.solve(
        Sets.union(outsideConstraints, Sets.union(accumulatedConstraints, externalConstraints)),
        basePath.resolve(name),
        target);
  }

  public Solver.Result solve(
      Set<Constraint> outsideConstraints, List<UnknownCoefficient> target, String suffix) {
    return Solver.solve(
        Sets.union(outsideConstraints, Sets.union(accumulatedConstraints, externalConstraints)),
        basePath.resolve(suffix),
        target);
  }

  public Obligation weaken(Obligation obligation) {
    final var result = apply(obligation, RULE_W);
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

  public void scheduleWeakenVariables(Obligation obligation, Stack<RuleSchedule> todo) {
    final var wvars = WVar.redundantIds(obligation).toList();
    for (int i = 0; i < wvars.size(); i++) {
      todo.push(RuleSchedule.schedule(RULE_W_VAR));
    }
  }

  public Obligation weakenVariables(Obligation obligation) {
    if (WVar.redundantId(obligation).isPresent()) {
      return weakenVariables(apply(obligation, RULE_W_VAR).get(0));
    }
    return obligation;
  }

  public void read(Obligation root, Path path) throws IOException {
    final var visitor = new Tactic(this, root);
    visitor.visitTactic(
        new TacticParser(new CommonTokenStream(new TacticLexer(CharStreams.fromPath(path))))
            .tactic());
  }

  public void addExternalConstraints(Collection<Constraint> external) {
    this.externalConstraints.addAll(external);
  }
}
