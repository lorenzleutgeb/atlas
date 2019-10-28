package xyz.leutgeb.lorenz.logs.ast;

import static com.google.common.collect.Sets.intersection;
import static xyz.leutgeb.lorenz.logs.Util.bug;
import static xyz.leutgeb.lorenz.logs.Util.indent;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.function.Function;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.ast.sources.Predefined;
import xyz.leutgeb.lorenz.logs.ast.sources.Renamed;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingContext;
import xyz.leutgeb.lorenz.logs.resources.AnnotatingGlobals;
import xyz.leutgeb.lorenz.logs.resources.Annotation;
import xyz.leutgeb.lorenz.logs.typing.TypeError;
import xyz.leutgeb.lorenz.logs.typing.types.TreeType;
import xyz.leutgeb.lorenz.logs.typing.types.Type;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@Data
@EqualsAndHashCode(callSuper = true)
public class LetExpression extends Expression {
  private final Identifier declared;
  private final Expression value;

  private final Expression body;

  public LetExpression(Source source, Identifier declared, Expression value, Expression body) {
    super(source);
    this.declared = declared;
    this.value = value;
    this.body = body;
  }

  public LetExpression(
      Source source, Identifier declared, Expression value, Expression body, Type type) {
    super(source, type);
    this.declared = declared;
    this.value = value;
    this.body = body;
  }

  @Override
  public Stream<? extends Expression> getChildren() {
    return Stream.concat(follow(), Stream.of(declared));
  }

  @Override
  public Stream<? extends Expression> follow() {
    return Stream.of(value, body);
  }

  @Override
  public Annotation inferAnnotationsInternal(AnnotatingContext gammaxq, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    final var constraints = globals.getConstraints();
    final var x = declared.getName();

    // \Gamma is used as context for e1, so from the combined context,
    // take \Gamma to be exactly the variables that occur in e1.
    final var varsForGamma = value.freeVariables();

    // \Delta on the other hand is "everything that's not in \Gamma".
    final var varsForDelta = body.freeVariables();
    Sets.difference(new HashSet<>(gammaxq.getIds()), varsForGamma).copyInto(varsForDelta);
    varsForDelta.remove(x);

    // Check for variables that are contained in both \Delta and \Gamma and apply (share).
    final var intersect = intersection(varsForGamma, varsForDelta);

    // NOTE: The following check only makes sense if we called "unshare" before, which is in flux.
    if (!intersect.isEmpty()) {
      throw new IllegalStateException(
          "assuming that you called unshare, there should be no intersections");
    }

    // final var sharing = gammaxq.share(this, constraints, new ArrayList<>(intersect));

    /** unshare final var q = sharing.getSecond(); */
    final var q = gammaxq;

    // TODO: This is an ugly mutation.
    /** unshare body = body.rename(sharing.getFirst()); */

    // Newly generated variables are being used in \Delta, while \Gamma uses old names.
    /**
     * unshare final var renamedVarsForDelta = varsForDelta.stream() .map(id ->
     * sharing.getFirst().getOrDefault(id, id)) .collect(Collectors.toSet());
     */
    final var xl = (value.getType() instanceof TreeType) ? 1 : 0;

    final var gamma = new ArrayList<>(varsForGamma);
    /** unshare final var delta = new ArrayList<>(renamedVarsForDelta); */
    final var delta = new ArrayList<>(varsForDelta);

    final var deltax = new ArrayList<>(delta);
    // For the case that the scrutinee (variable "x") is not a tree, we do not actually
    // add it to the context that R annotates, and it is the same as \Delta.
    if (xl > 0) {
      deltax.add(x);
    }

    final var gammap = new AnnotatingContext(gamma, constraints.heuristic(gamma.size()));
    final var deltaxr = new AnnotatingContext(deltax, constraints.heuristic(deltax.size()));

    // A. Rank Coefficients
    // p_i = q_i
    for (var id : gamma) {
      constraints.eq(this, gammap.getRankCoefficient(id), q.getRankCoefficient(id));
    }

    // r_j = q_{j + m}
    for (var id : delta) {
      constraints.eq(this, deltaxr.getRankCoefficient(id), q.getRankCoefficient(id));
    }

    // Keep track of all P_{\vec{b}}
    final Map<Map<String, Integer>, AnnotatingContext> pbs = new HashMap<>();

    // Produces a new annotating context to be used for cost-free typing.
    final Function<? super Map<String, Integer>, AnnotatingContext> pbProducer =
        (key) -> new AnnotatingContext(gamma, constraints.heuristic(gamma.size()));

    //  i. p_{(\vec{a}, c)} = q_{(\vec{a}, \vec{0}, c)}
    // ii. p^{\vec{b}}_{(\vec{a}, c)} = q_{(\vec{a}, \vec{b}, c)}
    q.streamIndices()
        // Filter to make sure that \vec{a} != \vec{0}.
        .filter(index -> gamma.stream().noneMatch(id -> index.getFirst().get(id) == 0))
        .forEach(
            index -> {
              // Check if the index for all variables from delta are zero.
              if (delta.stream().allMatch(id -> index.getFirst().get(id) == 0)) {
                // Case (i).
                // p_{(\vec{a}, c)} = q_{(\vec{a}, \vec{0}, c)}
                constraints.eq(this, q.getCoefficient(index), gammap.getCoefficient(index));
              } else {
                // Case (ii).
                // p^{\vec{b}}_{(\vec{a}, c)} = q_{(\vec{a}, \vec{b}, c)}
                constraints.eq(
                    this,
                    q.getCoefficient(index),
                    pbs.computeIfAbsent(index.getFirst(), pbProducer).getCoefficient(index));
              }
            });

    // p'^{\vec{b}}_{(a, c)} = r_{(\vec{b}, a, c)}
    for (var e : pbs.entrySet()) {
      final var b = e.getKey();
      final var pb = e.getValue();
      final var pbp = this.value.inferAnnotationsInternal(pb, globals.costFree());

      for (var d : pbp.getCoefficients()) {
        final var index = d.getKey();
        if (index.size() != xl + 1) {
          throw bug("index of expected size");
        }
        if (index.size() == 1) {
          final var value = d.getValue();
          final var c = index.get(index.size() - 1);
          constraints.eq(this, deltaxr.getCoefficient(b, c), value);
        } else {
          final var a = index.get(1);
          if (a == 0) {
            continue;
          }
          final var value = d.getValue();
          final var c = index.get(index.size() - 1);
          final var bExtended = new HashMap<>(b);
          bExtended.put(x, a);
          constraints.eq(this, deltaxr.getCoefficient(bExtended, c), value);
        }
      }
    }

    final var e1pp = value.inferAnnotations(gammap, globals);

    // p'_{(a, c)} = r_{(\vec{0}, a, c)}
    // Note that the following also works if P' actually is of length zero.
    for (var e : e1pp.getCoefficients()) {
      final var index = e.getKey();
      final var value = e.getValue();
      final var c = index.get(index.size() - 1);
      final var rIndex = new HashMap<String, Integer>();
      if (e1pp.size() > 0) {
        rIndex.put(x, index.get(0));
      }
      for (var id : delta) {
        rIndex.put(id, 0);
      }
      constraints.eq(this, value, deltaxr.getCoefficient(rIndex, c));
    }

    if (e1pp.size() > 1) {
      throw new RuntimeException("bug");
    }

    // p' = r_{k + 1}
    if (xl > 0) {
      globals.getConstraints().eq(this, e1pp.getRankCoefficient(), deltaxr.getRankCoefficient(x));
    }

    return body.inferAnnotations(deltaxr, globals);
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var declaredType = context.getProblem().fresh();
    context.getProblem().addIfNotEqual(this, declaredType, value.infer(context).wiggle(context));
    var sub = context.child();
    sub.putType(declared.getName(), declaredType);
    sub.getProblem().addIfNotEqual(this, declaredType, declared.infer(sub).wiggle(context));

    var result = context.getProblem().fresh();
    sub.getProblem().addIfNotEqual(this, result, body.infer(sub).wiggle(context));
    return result;
  }

  @Override
  public Expression normalize(Stack<Pair<Identifier, Expression>> context) {
    Stack<Pair<Identifier, Expression>> sub = new Stack<>();
    return new LetExpression(source, declared, value.normalize(sub), body).bindAll(sub);
  }

  @Override
  public Expression rename(Map<String, String> renaming) {
    return new LetExpression(
        new Renamed(source), declared, value.rename(renaming), body.rename(renaming), type);
  }

  @Override
  public void printTo(PrintStream out, int indentation) {
    out.print("let ");
    declared.printTo(out, indentation);
    out.print(" = ");
    value.printTo(out, indentation + 1);
    // out.println();
    // indent(out, indentation);
    out.println(" in (");
    indent(out, indentation);
    body.printTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print(")");
  }

  @Override
  public void printHaskellTo(PrintStream out, int indentation) {
    out.print("let ");
    declared.printHaskellTo(out, indentation);
    out.print(" = ");
    value.printHaskellTo(out, indentation + 1);
    // out.println();
    // indent(out, indentation);
    out.println(" in (");
    indent(out, indentation);
    body.printHaskellTo(out, indentation + 1);
    out.println();
    indent(out, indentation);
    out.print(")");
  }

  @Override
  public Set<String> freeVariables() {
    final var result = super.freeVariables();
    result.remove(declared.getName());
    return result;
  }

  @Override
  public String toString() {
    return "let " + declared + " = " + value + " in ...";
  }

  @Override
  public Expression unshare(Map<String, Integer> unshared) {
    final var intersection = intersection(value.freeVariables(), body.freeVariables());

    // Easy case, just recurse further down into the body.
    if (intersection.isEmpty()) {
      return new LetExpression(source, declared, value, body.unshare(unshared), type);
    }

    // Otherwise, there's some overlap between body and value.
    var target = new Identifier(Predefined.INSTANCE, intersection.iterator().next());
    var down = ShareExpression.clone(target, unshared);
    var result = ShareExpression.rename(target, down, Pair.create(value, body));

    var replacement =
        new ShareExpression(
            target,
            down,
            new LetExpression(
                source,
                declared,
                result.getFirst().unshare(unshared),
                result.getSecond().unshare(unshared),
                type));

    return intersection.size() > 1 ? replacement.unshare(unshared) : replacement;
  }
}
