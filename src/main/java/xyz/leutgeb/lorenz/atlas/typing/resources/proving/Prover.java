package xyz.leutgeb.lorenz.atlas.typing.resources.proving;

import static java.util.Collections.emptyList;
import static java.util.Collections.emptyMap;
import static java.util.Collections.emptySet;
import static java.util.function.Function.identity;
import static java.util.stream.Collectors.toUnmodifiableMap;
import static xyz.leutgeb.lorenz.atlas.ast.expressions.IdentifierExpression.isLeaf;
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
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.atlas.antlr.TacticLexer;
import xyz.leutgeb.lorenz.atlas.antlr.TacticParser;
import xyz.leutgeb.lorenz.atlas.ast.*;
import xyz.leutgeb.lorenz.atlas.ast.expressions.*;
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
  private static final Rule RULE_MATCH_TUPLE = MatchTuple.INSTANCE;
  private static final Rule RULE_NODE = Node.INSTANCE;
  private static final Rule RULE_SHARE = Share.INSTANCE;
  private static final Rule RULE_VAR = Var.INSTANCE;
  private static final Rule RULE_W_VAR = WVar.INSTANCE;
  private static final Rule RULE_W = W.INSTANCE;
  private static final Rule RULE_LEAF = Leaf.INSTANCE;
  private static final Rule RULE_SHIFT = Shift.INSTANCE;
  private static final Rule RULE_TICK = Tick.INSTANCE;
  private static final Rule RULE_TICK_DEFER = TickDefer.INSTANCE;

  private static final Map<String, Rule> RULES_BY_NAME =
      Stream.of(
              RULE_LET_TREE_CF,
              RULE_APP,
              RULE_CMP,
              RULE_ITE,
              RULE_MATCH,
              RULE_MATCH_TUPLE,
              RULE_NODE,
              RULE_SHARE,
              RULE_VAR,
              RULE_W_VAR,
              RULE_W,
              RULE_LEAF,
              RULE_SHIFT,
              RULE_TICK,
              RULE_TICK_DEFER)
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
  private final DirectedAcyclicGraph<Obligation, IndexedEdge> proof =
      new DirectedAcyclicGraph<>(IndexedEdge.class);

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

  private final boolean naive = false;

  public record RuleSchedule(Rule rule, Map<String, String> arguments, Optional<String> comment) {
    public static RuleSchedule schedule(Rule rule) {
      return new RuleSchedule(rule, emptyMap(), Optional.empty());
    }

    public static RuleSchedule schedule(Rule rule, Map<String, String> arguments) {
      return new RuleSchedule(rule, arguments, Optional.empty());
    }

    public static RuleSchedule schedule(Rule rule, Map<String, String> arguments, String comment) {
      return new RuleSchedule(rule, arguments, Optional.ofNullable(comment));
    }

    @Override
    public String toString() {
      var result = rule.getName();

      if (arguments.isEmpty()) {
        return result;
      }

      result +=
          arguments.entrySet().stream()
              .filter(e -> !e.getValue().equals("false"))
              .map(Object::toString)
              .map(s -> s.replaceAll("=true$", ""))
              .sorted()
              .collect(Collectors.joining(" ", "{", "}"));

      if (comment.isEmpty()) {
        return result;
      }

      return result + " (* " + comment.get() + " *)";
    }
  }

  public Prover(String name, AnnotatingGlobals globals, Path basePath) {
    load(basePath.resolve("z3.log"));
    this.name = name;
    this.globals = globals;
    this.basePath = basePath;
  }

  /** Chooses which rules should be applied (in order) to prove given obligation. */
  private List<RuleSchedule> chooseRules(Obligation obligation) {
    final var expression = obligation.getExpression();
    if (expression.isTerminal()) {
      final List<RuleSchedule> todo = new ArrayList<>();
      int beforeWeakeningVars = todo.size();
      scheduleWeakenVariables(obligation, todo);
      if (todo.size() > beforeWeakeningVars) {
        todo.add(RuleSchedule.schedule(RULE_W));
      }
      if (weakenBeforeTerminal || weakenAggressively) {
        todo.add(RuleSchedule.schedule(RULE_W));
      }
      if (expression instanceof CallExpression && TICK_BEFORE_APP) {
        log.trace("Automatically applying (tick) to expression `{}`!", expression);
        if (Boolean.parseBoolean(Util.getProperty(Prover.class, "tickDefer", "true"))) {
          todo.add(RuleSchedule.schedule(RULE_TICK_DEFER));
        } else {
          todo.add(RuleSchedule.schedule(RULE_TICK));
        }
      }
      todo.add(RuleSchedule.schedule(chooseRule(expression)));
      return todo;
    } else if (weakenAggressively) {
      log.trace(
          "Automatically applying (w) to expression `{}` because aggressive weakening is enabled!",
          expression);
      return List.of(RuleSchedule.schedule(RULE_W), RuleSchedule.schedule(chooseRule(expression)));
    } else {
      return List.of(RuleSchedule.schedule(chooseRule(expression)));
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
    } else if (e instanceof TupleExpression tuple) {
      return tuple.getTree().map(IdentifierExpression::isLeaf).orElse(false) ? RULE_LEAF : RULE_VAR;
    } else if (e instanceof IdentifierExpression identifier) {
      return isLeaf(identifier) ? RULE_LEAF : RULE_VAR;
    } else if (e instanceof IfThenElseExpression) {
      return RULE_ITE;
    } else if (e instanceof MatchTreeExpression) {
      return RULE_MATCH;
    } else if (e instanceof MatchTupleExpression) {
      return RULE_MATCH_TUPLE;
    } else if (e instanceof LetExpression letExpression) {
      if (letExpression.getValue().getType().countTrees().get() > 0) {
        // TODO(lorenzleutgeb): When can we apply let:tree?
        return RULE_LET_TREE_CF;
      }
    } else if (e instanceof ShareExpression) {
      return RULE_SHARE;
    } else if (e instanceof TickExpression) {
      if (Boolean.parseBoolean(Util.getProperty(Prover.class, "tickDefer", "true"))) {
        return RULE_TICK_DEFER;
      } else {
        return RULE_TICK;
      }
    }
    throw bug("could not choose a rule for expression of type " + e.getClass().getCanonicalName());
  }

  private static boolean firstAfterCall(Expression expression) {
    final var parent = expression.getParent();

    if (parent == null) {
      return false;
    }

    IdentifierExpression returned;
    if (parent instanceof MatchTreeExpression match) {
      if (match.getNode() != expression) {
        return false;
      }
      returned = (IdentifierExpression) match.getScrut();
    } else {
      return false;
    }

    return assignedFromCall(returned, parent);
  }

  private static boolean callsOrTicks(Expression e) {
    if (e instanceof TickExpression tick) {
      return callsOrTicks(tick.getBody());
    }
    return e instanceof CallExpression;
  }

  private static boolean assignedFromCall(IdentifierExpression identifier, Expression context) {
    final var parent = context.getParent();

    if (parent == null) {
      return false;
    }

    if (parent instanceof LetExpression let) {
      if (let.getDeclared().equals(identifier)) {
        if (callsOrTicks(let.getValue())) {
          return true;
        }
      }
    }

    return assignedFromCall(identifier, parent);
  }

  private List<RuleSchedule> auto(Obligation obligation, FunctionDefinition fd) {
    final var e = obligation.getExpression();
    final Expression parent = e.getParent();

    boolean size = false;
    boolean mono = false;
    boolean l2xy = false;
    boolean lp1 = false;
    boolean weaken = false;
    boolean neg = false;

    List<String> comments = new ArrayList<>();

    if (firstAfterCall(e)) {
      comments.add("first after call");
      if (e.isTreeConstruction()) {
        comments.add("l2xy for tree construction");
        l2xy = true;
        mono = true;
      } else {
        // l2xy = true;
      }
    }

    if (e instanceof CallExpression) {
      final var isInLet = parent instanceof LetExpression;
      final var isInTick = parent instanceof TickExpression;

      if (!isInLet && !isInTick) {
        comments.add("l2xy for call not in let or tick");
      }
    } else {
      if (e instanceof IdentifierExpression || e instanceof TupleExpression) {
        if (!(parent instanceof LetExpression let && let.getValue() == e)) {
          comments.add("proof leaf 1");
          mono = true;
        }
      } else if (e instanceof NodeExpression) {
        if (parent != null) {
          if (parent instanceof LetExpression let) {
            if (let.getBody() == e) {
              comments.add("proof leaf 2");
              mono = true;
            }
          } else {
            comments.add("proof leaf 3");
            weaken = true;
            mono = true;
          }
        }
      } else if (e instanceof final LetExpression letExpression) {
        final var value = letExpression.getValue();
        if (value instanceof CallExpression || value instanceof TickExpression) {
          // Makes sure we apply (w{l2xy}) right before (app) or (tick).
          comments.add("binds a call/tick");
          l2xy = true;
          mono = true;
          if (value.getOccurringFunctions().contains(fd.getFullyQualifiedName())) {
            neg = true;
          }
        } else if (letExpression.isTreeConstruction()) {
          if (parent instanceof MatchTreeExpression || parent instanceof IfThenElseExpression) {
            comments.add("first after match");
            mono = true;
          }
        }
      } else if (e instanceof TickExpression) {
        if (!(parent instanceof LetExpression)) {
          comments.add("tick outside let");
          mono = true;
        }
      } else if (e instanceof IfThenElseExpression ite && Expression.isCoin(ite.getCondition())) {
        comments.add("before ite:coin");
        l2xy = true;
      }
    }

    final List<RuleSchedule> todo = new Stack<>();

    if (size) {
      // size implies mono, so we don't need to set it.
      mono = false;
    }

    if (mono || size || l2xy || lp1 || weaken) {
      todo.add(
          RuleSchedule.schedule(
              RULE_W,
              Map.of(
                  "mono",
                  String.valueOf(mono),
                  "size",
                  String.valueOf(size),
                  "l2xy",
                  String.valueOf(l2xy),
                  "lp1",
                  String.valueOf(lp1),
                  "neg",
                  String.valueOf(neg)),
              String.join(", ", comments)));
    }

    if (e.isTerminal()) {
      scheduleWeakenVariables(obligation, todo);
    }

    if (e instanceof CallExpression call) {
      // if (!call.getFullyQualifiedName().equals(fd.getFullyQualifiedName())) {
      todo.add(RuleSchedule.schedule(RULE_SHIFT));
      // }
      if (TICK_BEFORE_APP) {
        log.trace("Automatically applying (tick) to expression `{}`!", e);
        todo.add(RuleSchedule.schedule(RULE_TICK));
      }
    }

    if (e instanceof LetExpression && neg) {
      todo.add(RuleSchedule.schedule(RULE_LET_TREE_CF, Map.of("nege", "true")));
    } else {
      todo.add(RuleSchedule.schedule(chooseRule(e)));
    }

    return todo;
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

  public void proveFrom(Obligation obligation, FunctionDefinition fd, Obligation root) {
    final var schedules = vertexAttributes.get(root);
    if (schedules == null) {
      log.warn("Skipping {}", obligation);
      return;
    }
    final var ruleResult = applyInternal(obligation, schedules.schedule);
    var edges = proof.outgoingEdgesOf(root).stream().sorted().toList();
    if (edges.size() != ruleResult.obligations().size()) {
      throw bug("");
    }
    for (int i = 0; i < edges.size(); i++) {
      proveFrom(ruleResult.obligations().get(i), fd, proof.getEdgeTarget(edges.get(i)));
    }
  }

  /** Translates given obligation into a set of constraints that would prove given obligation. */
  public void prove(Obligation obligation, FunctionDefinition fd) {
    Stack<Obligation> obligations = new Stack<>();
    obligations.push(obligation);

    while (!obligations.isEmpty()) {
      final var top = obligations.pop();
      final var schedules = auto(top, fd);

      var nextObligation = top;
      ApplicationResult ruleResult = null;

      for (Iterator<RuleSchedule> it = schedules.iterator(); it.hasNext(); ) {
        final var schedule = it.next();
        ruleResult = applyInternal(nextObligation, schedule);

        if (it.hasNext()) {
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

    for (int i = 0; i < ruleResult.obligations().size(); i++) {
      proof.addEdge(previous, ruleResult.obligations().get(i));
      proof.getEdge(previous, ruleResult.obligations().get(i)).setIndex(i);
    }

    ruleResult.collectInto(accumulatedConstraints);
  }

  public void printTactic(Obligation obligation, PrintStream out, boolean costOnly) {
    if (!proof.containsVertex(obligation)) {
      throw new IllegalArgumentException("unknown obligation");
    }

    printTactic(obligation, costOnly, out, 0);
  }

  private void printTactic(
      Obligation obligation, boolean costOnly, PrintStream out, int indentation) {
    if (costOnly && !obligation.isCost()) {
      return;
    }
    final var meta = vertexAttributes.get(obligation);
    if (meta == null) {
      return;
    }
    final var leaf = meta.constraints.isEmpty();

    indent(out, indentation);
    if (!leaf) {
      out.print("(");
    }

    out.print(meta.schedule());
    if (obligation.getExpression() instanceof IfThenElseExpression ifThenElseExpression) {
      if (ifThenElseExpression.getCondition().isTerminal()) {
        out.print(" (* " + ifThenElseExpression.getCondition() + " *)");
      }
    } else if (obligation.getExpression() instanceof MatchTreeExpression matchExpression) {
      if (matchExpression.getScrut().getSource() instanceof Derived derived) {
        out.print(" (* " + derived.getParent() + " *)");
      } else if (matchExpression.getScrut().isTerminal()) {
        out.print(" (* " + matchExpression.getScrut() + " *)");
      }
    }

    if (!leaf) {
      out.println();
    }

    final var outgoing = proof.outgoingEdgesOf(obligation).stream().sorted().toList();
    for (final IndexedEdge edge : outgoing) {
      printTactic(proof.getEdgeTarget(edge), costOnly, out, indentation + 1);
    }

    if (!leaf) {
      indent(out, indentation);
      out.println(")");
    } else {
      out.println();
    }
  }

  public void plot(Obligation root) {
    if (basePath == null) {
      log.warn("Cannot plot without base path.");
      return;
    }

    try {
      final NidiExporter<Obligation, IndexedEdge> exporter = new NidiExporter<>(Util::stamp);
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

      final NidiExporter<Obligation, IndexedEdge> exporter = new NidiExporter<>(Util::stamp);
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

  public void scheduleWeakenVariables(Obligation obligation, List<RuleSchedule> todo) {
    final var wvars = WVar.redundantIds(obligation).toList();
    for (int i = 0; i < wvars.size(); i++) {
      todo.add(RuleSchedule.schedule(RULE_W_VAR));
    }
  }

  public Obligation weakenVariables(Obligation obligation) {
    if (WVar.redundantId(obligation).isPresent()) {
      return weakenVariables(apply(obligation, RULE_W_VAR).get(0));
    }
    return obligation;
  }

  public void read(Obligation root, Path path, FunctionDefinition fd) throws IOException {
    final var visitor = new Tactic(this, root, fd);
    visitor.visitTactic(
        new TacticParser(new CommonTokenStream(new TacticLexer(CharStreams.fromPath(path))))
            .tactic());
  }

  public void addExternalConstraints(Collection<Constraint> external) {
    this.externalConstraints.addAll(external);
  }
}
