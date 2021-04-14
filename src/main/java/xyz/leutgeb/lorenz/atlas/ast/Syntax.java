package xyz.leutgeb.lorenz.atlas.ast;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import xyz.leutgeb.lorenz.atlas.ast.sources.Source;

@RequiredArgsConstructor
public abstract class Syntax {
  @Getter protected final Source source;
}
