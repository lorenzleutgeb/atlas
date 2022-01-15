package xyz.leutgeb.lorenz.atlas.typing.resources.proving;

import org.jgrapht.graph.DefaultEdge;

public class IndexedEdge extends DefaultEdge implements Comparable<IndexedEdge> {
  private int index;

  public IndexedEdge(int index) {
    this.index = index;
  }

  public IndexedEdge() {}

  @Override
  public int compareTo(IndexedEdge o) {
    return Integer.compare(this.index, o.index);
  }

  public void setIndex(int i) {
    this.index = i;
  }
}
