# Automated Analysis of Logarithmic Amoritzed Complexity

See [arXiv:1807.08242](https://arxiv.org/abs/1807.08242).

## Typechecker

### Interpreting results

The size of a tree `t`, denoted as `|t|` is defined to be the number of its leaves. That, is `|nil| := 1` as well as
`|(t₁, _, t₂)| := |t₁| + |t₂|`.

### Dependencies

All dependencies except the [The Z3 Theorem Prover](https://github.com/Z3Prover/z3)
are managed and automatically downloaded/installed by [Gradle](https://gradle.org/)
(see the section labelled `dependencies` in `build.gradle.kts`).

#### The Z3 Theorem Prover

Refer to https://github.com/Z3Prover/z3#java

To compile against Z3, make sure that `com.microsoft.z3.jar` exists in
`typechecker/libs`. This Java archive should contain all classes from the
`com.microsoft.z3` package which implements the Java API for Z3.

At runtime, `libz3.so` and `libz3java.so` must be available on the system.
To find out how to build these libraries, please refer to Z3 docs.
Lookup of shared libraries is platform dependent, but on Linux it
should be sufficient to place the files in any of the following `:`-separated
locations: `/usr/java/packages/lib:/usr/lib64:/lib64:/lib:/usr/lib`.
If there is an issue loading Z3, the program will show a
`java.lang.UnsatisfiedLinkError` and terminate with a non-zero exit code.

## Interpreter

```
$ cabal run
```

## Reading

 - [Mechanically Proving Termination Using Polynomial Interpretations](https://doi.org/10.1007/s10817-005-9022-x)
 - [“Carbon Credits” for Resource-Bounded Computations Using Amortised Analysis](https://doi.org/10.1007/978-3-642-05089-3_23)
 - ...

