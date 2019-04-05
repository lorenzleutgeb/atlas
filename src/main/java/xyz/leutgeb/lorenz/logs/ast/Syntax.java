package xyz.leutgeb.lorenz.logs.ast;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public abstract class Syntax {
  @Getter protected final Source source;
}
