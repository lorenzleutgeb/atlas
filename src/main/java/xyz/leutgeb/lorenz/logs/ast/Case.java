package xyz.leutgeb.lorenz.logs.ast;

import lombok.EqualsAndHashCode;
import lombok.NonNull;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class Case extends Syntax {
  @NonNull Expression matcher;
  @NonNull Expression body;

  public Case(Source source, @NonNull Expression matcher, @NonNull Expression body) {
    super(source);
    this.matcher = matcher;
    this.body = body;
  }

  public Case normalize() {
    if (!matcher.isImmediate()) {
      throw new UnsupportedOperationException("non-immediate patterns are not supported");
    }

    return new Case(source, matcher, body.normalizeAndBind());
  }
}
