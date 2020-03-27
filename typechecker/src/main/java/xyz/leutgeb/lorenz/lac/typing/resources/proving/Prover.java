package xyz.leutgeb.lorenz.lac.typing.resources.proving;

import static xyz.leutgeb.lorenz.lac.Util.bug;
import static xyz.leutgeb.lorenz.lac.Util.stack;

import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.stream.Collectors;
import lombok.extern.log4j.Log4j2;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.graph.DirectedAcyclicGraph;
import org.jgrapht.nio.Attribute;
import org.jgrapht.nio.AttributeType;
import org.jgrapht.nio.DefaultAttribute;
import xyz.leutgeb.lorenz.lac.NidiAttribute;
import xyz.leutgeb.lorenz.lac.NidiExporter;
import xyz.leutgeb.lorenz.lac.Util;
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
import xyz.leutgeb.lorenz.lac.typing.resources.constraints.Constraint;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.App;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Cmp;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Ite;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Let;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Match;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Node;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Rule;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Share;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.Var;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.W;
import xyz.leutgeb.lorenz.lac.typing.resources.rules.WVar;

@Log4j2
public class Prover {
  private static final Let letRule = new Let();
  private static final App applicationRule = new App();
  private static final Cmp comparisonRule = new Cmp();
  private static final Ite ifThenElseRule = new Ite();
  private static final Match matchRule = new Match();
  private static final Node nodeRule = new Node();
  private static final Share shareRule = new Share();
  private static final Var variableRule = new Var();
  private static final WVar weakenVariableRule = new WVar();
  private static final W weakenRule = new W();

  /** Translates given obligation into a set of constraints that would prove given obligation. */
  public static Set<Constraint> prove(
      Obligation obligation, AnnotatingGlobals globals, OutputStream out) {
    DirectedAcyclicGraph<Obligation, DefaultEdge> proof =
        new DirectedAcyclicGraph<>(DefaultEdge.class);

    Set<Constraint> accumulatedConstraints = new HashSet<>();
    Stack<Obligation> obligations = new Stack<>();
    obligations.push(obligation);
    proof.addVertex(obligation);

    final var edgeAttributes = new HashMap<DefaultEdge, Map<String, Attribute>>();
    final var vertexAttributes = new HashMap<Obligation, Map<String, Attribute>>();
    vertexAttributes.put(obligation, obligation.attributes());

    while (!obligations.isEmpty()) {
      final var top = obligations.pop();
      final var rules = chooseRules(top);

      var nextObligation = top;
      Rule.RuleApplicationResult ruleResult = null;

      while (!rules.isEmpty()) {
        final var rule = rules.pop();
        final var currentObligation = nextObligation;
        ruleResult = rule.apply(nextObligation, globals);

        if (ruleResult == null) {
          throw bug("typing rule implementation returned null");
        }

        ruleResult
            .getObligations()
            .forEach(
                o -> {
                  proof.addVertex(o.getFirst());
                  vertexAttributes.put(o.getFirst(), o.getFirst().attributes());
                });
        final var lele = ruleResult.getGeneralConstraints();
        ruleResult
            .getObligations()
            .forEach(
                o -> {
                  final var edge = proof.addEdge(currentObligation, o.getFirst());
                  final var lel =
                      o.getSecond().stream()
                              .map(Object::toString)
                              .collect(Collectors.joining("<br />"))
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
                                          + rule.getClass().getSimpleName().toLowerCase()
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
                  accumulatedConstraints.addAll(o.getSecond());
                });

        accumulatedConstraints.addAll(ruleResult.getGeneralConstraints());

        if (!rules.isEmpty()) {
          if (ruleResult.getObligations().size() > 1) {
            throw bug(
                "if there are multiple rule applications scheduled, all of them except the last must return exactly one obligation");
          }

          if (ruleResult.getObligations().isEmpty()) {
            throw bug("multiple rule applications were scheduled but we ran out of obligations");
          }

          nextObligation = ruleResult.getObligations().get(0).getFirst();
        } else {
          ruleResult.getObligations().forEach(o -> obligations.add(o.getFirst()));
        }
      }
    }

    final NidiExporter<Obligation, DefaultEdge> exporter = new NidiExporter<>(Util::stamp);
    exporter.setVertexAttributeProvider(vertexAttributes::get);
    exporter.setEdgeAttributeProvider(edgeAttributes::get);
    exporter.setGraphAttributeProvider(
        () -> Map.of("rankdir", new DefaultAttribute<>("BT", AttributeType.STRING)));
    final var graph = exporter.transform(proof);
    final var viz = Graphviz.fromGraph(graph);
    try {
      viz.render(Format.SVG).toOutputStream(out);
    } catch (Exception e) {
      e.printStackTrace();
    }
    return accumulatedConstraints;
  }

  /** Chooses which rules should be applied (in order) to prove given obligation. */
  private static Stack<Rule> chooseRules(Obligation obligation) {
    if (obligation.isNothing()) {
      return stack();
    }
    final var expression = obligation.getExpression();
    if (!WVar.redundantIds(obligation).isEmpty()) {
      return stack(weakenVariableRule);
    }
    if (expression.isTerminal()) {
      return stack(weakenRule, chooseRule(expression));
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
      return variableRule;
    } else if (e instanceof IfThenElseExpression) {
      return ifThenElseRule;
    } else if (e instanceof MatchExpression) {
      return matchRule;
    } else if (e instanceof LetExpression) {
      return letRule;
    } else if (e instanceof ShareExpression) {
      return shareRule;
    } else {
      throw bug("could not choose a rule");
    }
  }
}
