package xyz.leutgeb.lorenz.lac.ast;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.lac.ast.sources.Source;

@RequiredArgsConstructor
public abstract class Syntax {
  @Getter protected final Source source;
}
