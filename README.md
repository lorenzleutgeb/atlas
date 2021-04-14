# lac

[![DOI](https://zenodo.org/badge/156873559.svg)](https://zenodo.org/badge/latestdoi/156873559)
[![codecov](https://codecov.io/gh/lorenzleutgeb/lac/branch/main/graph/badge.svg?token=cXfOoGOXV2)](https://codecov.io/gh/lorenzleutgeb/lac)
[![codebeat badge](https://codebeat.co/badges/64e5fc79-f2b7-4a49-ac3e-bf19395d1b07)](https://codebeat.co/projects/github-com-lorenzleutgeb-lac-main)

A tool for Automated Analysis of **L**ogarithmic **A**moritzed **C**omplexity

See [arXiv:1807.08242](https://arxiv.org/abs/1807.08242).


## Interpreting results

The size of a tree `t`, denoted as `|t|` is defined to be the number of its leaves. That, is `|nil| := 1` as well as
`|(t₁, _, t₂)| := |t₁| + |t₂|`.

## Properties

Behaviour of the system can be controlled by setting properties.

Z3 specific configuration can be set via `com.microsoft.z3.*`, for example `com.microsoft.z3.memory_max_size=25769803776` or `com.microsoft.z3.unsat_core=true`.
Logging-specific configuration can be set via `org.slf4j.simpleLogger.*`.
Tool-specific configuration can be set via `xyz.leutgeb.lorenz.lac.*`.

## Usage

Some of the above properties are exposed as command-line arugments.

When inferring, set minimization target (`--minimize={roots,all}`).
Fix right sides (`--mode={amortized,worstcase,free}`).

## Dependencies

All dependencies except the [The Z3 Theorem Prover](https://github.com/Z3Prover/z3)
are managed and automatically downloaded/installed by [Gradle](https://gradle.org/)
(see the section labelled `dependencies` in `build.gradle.kts`).

### The Z3 Theorem Prover

Refer to https://github.com/Z3Prover/z3#java

To compile against Z3, make sure that `com.microsoft.z3.jar` exists in
`lib`. This Java archive should contain all classes from the
`com.microsoft.z3` package which implements the Java API for Z3.

At runtime, `libz3.so` and `libz3java.so` must be available on the system.
To find out how to build these libraries, please refer to Z3 docs.
Lookup of shared libraries is platform dependent, but on Linux it
should be sufficient to place the files in any of the following `:`-separated
locations: `/usr/java/packages/lib:/usr/lib64:/lib64:/lib:/usr/lib`.
If there is an issue loading Z3, the program will show a
`java.lang.UnsatisfiedLinkError` and terminate with a non-zero exit code.

## Related Repositories

 - [`lorenzleutgeb/lac-examples`](https://github.com/lorenzleutgeb/lac-examples)
 - [`lorenzleutgeb/lac-thesis`](https://github.com/lorenzleutgeb/lac-thesis)
 - [`lorenzleutgeb/lac-hs`](https://github.com/lorenzleutgeb/lac-hs)

## Reading

 - [Mechanically Proving Termination Using Polynomial Interpretations](https://doi.org/10.1007/s10817-005-9022-x)
 - [“Carbon Credits” for Resource-Bounded Computations Using Amortised Analysis](https://doi.org/10.1007/978-3-642-05089-3_23)
 - ...

