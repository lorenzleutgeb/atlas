package xyz.leutgeb.lorenz.logs.ast;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.logs.ast.sources.Source;

@RequiredArgsConstructor
public abstract class Syntax {
  @Getter protected final Source source;
}
