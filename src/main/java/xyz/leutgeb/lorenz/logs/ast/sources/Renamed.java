package xyz.leutgeb.lorenz.logs.ast.sources;

import lombok.EqualsAndHashCode;
import lombok.Value;

@Value
@EqualsAndHashCode(callSuper = true)
public class Renamed extends Source {
  Source parent;
}
