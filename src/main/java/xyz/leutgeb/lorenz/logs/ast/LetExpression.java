package xyz.leutgeb.lorenz.logs.ast;

import static xyz.leutgeb.lorenz.logs.Util.bug;
import static xyz.leutgeb.lorenz.logs.Util.indent;

import com.google.common.collect.Sets;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hipparchus.util.Pair;
import xyz.leutgeb.lorenz.logs.Context;
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
  private /*final*/ Expression body;

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
    return Stream.of(declared, value, body);
  }

  @Override
  public Annotation inferAnnotationsInternal(AnnotatingContext qin, AnnotatingGlobals globals)
      throws UnificationError, TypeError {
    final var constraints = globals.getConstraints();

    // Gamma is used as context for e1, so from the combined context,
    // take Gamma to be exactly the variables that occur in e1.
    // Delta on the other hand is "everything that's not in Gamma".
    var varsForDelta = body.freeVariables();
    varsForDelta.remove(declared.getName());
    final var varsForGamma = value.freeVariables();

    final var intersect = Sets.intersection(varsForGamma, varsForDelta);
    // if (!intersect.isEmpty()) {
    //  throw bug("(share) is not implemented. the set of shared variables is " + intersect);
    // }

    final var sharing = qin.share(this, constraints, new ArrayList<>(intersect));

    final var q = sharing.getSecond();
    body = body.rename(sharing.getFirst());
    varsForDelta =
        varsForDelta
            .stream()
            .map(id -> sharing.getFirst().getOrDefault(id, id))
            .collect(Collectors.toSet());

    /*
    final var m = varsInGamma.size();
    final var k = q.size() - m;
    */

    final var x = declared.getName();
    final var xl = (value.getType() instanceof TreeType) ? 1 : 0;
    final var e2l = (body.getType() instanceof TreeType) ? 1 : 0;
    final var e1l = (body.getType() instanceof TreeType) ? 1 : 0;

    final var gamma = new ArrayList<>(varsForGamma);
    final var delta = new ArrayList<>(varsForDelta);
    final var deltax = new ArrayList<>(delta);

    if (xl > 0) {
      deltax.add(declared.getName());
    }

    final var gammap = new AnnotatingContext(gamma, constraints.heuristic(gamma.size()));
    final var deltaxr = new AnnotatingContext(deltax, constraints.heuristic(deltax.size()));

    // The addition accounts for the case where the variable we are binding here is a tree
    // (and therefore carries potential).

    // A. Rank Coefficients
    // p_i = q_i
    for (var id : gamma) {
      constraints.eq(this, gammap.getRankCoefficient(id), q.getRankCoefficient(id));
    }

    // r_j = q_j
    // TODO: This looks dubious... Shouldn't it be r_{j} = q_{j + m}?
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
        // TODO: Filter to make sure that \vec{a} != \vec{0}.
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

    // TODO: Clarify. This can only work if x is a tree?
    // p' = r_{k + 1}
    if (xl > 0) {
      globals.getConstraints().eq(this, e1pp.getRankCoefficient(), deltaxr.getRankCoefficient(x));
    }

    // r_{(\vec{b}, 0, c)} = q_{(\vec{0}, \vec{b}, c)}
    /*
    for (var e : r.getCoefficients()) {
      final var index = e.getKey();
      if (index.get(index.size() - 2) != 0) {
        continue;
      }
      // We have to subtract 1 + xl here (0 is full index, 1 is full index
      // without c), now if x is a tree, then we subtract one more.
      final var b = index.subList(0, index.size() - (1 + xl));

      if (isZero(b)) {
         continue;
      }

      // We have to subtract one here, since c is _always_ the last element.
      final var c = index.get(index.size() - 1);
      final var value = e.getValue();
      // To create the long index we have to fill up with as many zero as
      // the difference between b's (forgetting c since it's on both sides) and
      // q.
      final var longIndex = zero((q.size() + 1) - b.size());
      longIndex.addAll(b);
      longIndex.add(c);
      constraints.eq(this, value, q.getAnnotation().getCoefficient(longIndex));
    }
    */

    return body.inferAnnotations(deltaxr, globals);
  }

  @Override
  public Type inferInternal(Context context) throws UnificationError, TypeError {
    var declaredType = context.getProblem().fresh();
    context.getProblem().add(this, declaredType, value.infer(context));
    var sub = context.child();
    sub.putType(declared.getName(), declaredType);
    sub.getProblem().add(this, declaredType, declared.infer(sub));

    var result = context.getProblem().fresh();
    sub.getProblem().add(this, result, body.infer(sub));
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
  public Set<String> freeVariables() {
    final var result = super.freeVariables();
    result.remove(declared.getName());
    return result;
  }
}
