package xyz.leutgeb.lorenz.logs.ast;

import com.google.common.collect.Interner;
import com.google.common.collect.Interners;
import java.util.Objects;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NonNull;
import xyz.leutgeb.lorenz.logs.Context;
import xyz.leutgeb.lorenz.logs.type.TreeType;
import xyz.leutgeb.lorenz.logs.type.Type;
import xyz.leutgeb.lorenz.logs.type.TypeError;
import xyz.leutgeb.lorenz.logs.unification.UnificationError;

@EqualsAndHashCode(callSuper = false)
public class Identifier extends TupleElement {
  public static final Identifier NIL = new Identifier("nil");
  public static final Identifier TRUE = new Identifier("true");
  public static final Identifier FALSE = new Identifier("false");

  private static final Interner<Identifier> INTERNER = Interners.newWeakInterner();

  @NonNull @Getter private final String name;

  static {
    INTERNER.intern(NIL);
    INTERNER.intern(TRUE);
    INTERNER.intern(FALSE);
  }

  private Identifier(String name) {
    Objects.requireNonNull(name);
    this.name = name;
  }

  public static Identifier get(String name) {
    return INTERNER.intern(new Identifier(name));
  }

  @Override
  public String toString() {
    return "(id " + name + ")";
  }

  @Override
  public Type infer(Context context) throws UnificationError, TypeError {
    if (this == NIL) {
      return new TreeType(context.getProblem().fresh());
    }

    Type ty = context.lookup(this.name);
    if (ty == null) {
      throw new TypeError.NotInContext(this.name);
    } else {
      return ty;
    }
  }
}
