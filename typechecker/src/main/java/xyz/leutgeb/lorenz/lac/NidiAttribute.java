package xyz.leutgeb.lorenz.lac;

import static org.jgrapht.nio.AttributeType.UNKNOWN;

import guru.nidi.graphviz.attribute.Attributes;
import guru.nidi.graphviz.attribute.For;
import lombok.Value;
import org.jgrapht.nio.AttributeType;

@Value
public class NidiAttribute<T extends Attributes<F>, F extends For>
    implements org.jgrapht.nio.Attribute {
  T value;

  public NidiAttribute(T value) {
    this.value = value;
  }

  public T getNidi() {
    return value;
  }

  @Override
  public String getValue() {
    return String.valueOf(value);
  }

  @Override
  public String toString() {
    return String.valueOf(value);
  }

  @Override
  public AttributeType getType() {
    return UNKNOWN;
  }
}
