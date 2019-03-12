package xyz.leutgeb.lorenz.logs.resources;

import java.util.HashMap;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
public class ResourceFunctionCoefficients {
  private final HashMap<Integer, HashMap<Integer, Integer>> coefficients;

  public int get(int a, int b) {
    var tmp = coefficients.get(a);
    if (tmp == null) {
      return 0;
    }
    return tmp.getOrDefault(a, 0);
  }

  public void set(int a, int b, int coefficient) {
    if (coefficient == 0) {
      return;
    }
    var tmp = coefficients.get(a);
    if (tmp == null) {
      tmp = new HashMap<>();
      tmp.put(b, coefficient);
      coefficients.put(a, tmp);
    } else {
      tmp.put(b, coefficient);
    }
  }
}
