package xyz.leutgeb.lorenz.logs;

import java.io.PrintStream;

public class Util {
  public static String generateSubscript(int i) {
    StringBuilder sb = new StringBuilder();
    for (char ch : String.valueOf(i).toCharArray()) {
      sb.append((char) ('\u2080' + (ch - '0')));
    }
    return sb.toString();
  }

  public static void indent(PrintStream out, int indentation) {
    for (int i = 0; i < indentation; i++) {
      out.print("    ");
    }
  }
}
