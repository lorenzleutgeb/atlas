{
  description = "lac";

  inputs.nixpkgs = { url = "nixpkgs/nixos-20.09"; };

  /* This flake has a more recent version of Graal that supports Java 11. */
  inputs.glittershark = { url = "github:glittershark/nixpkgs/graalvm-ce"; };

  outputs = { self, nixpkgs, glittershark }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      glittersharkPkgs = import glittershark { inherit system; };
    in rec {
      devShell."${system}" =
        with pkgs;
        mkShell {
          buildInputs = [
            dot2tex
            glittersharkPkgs.graalvm11-ce
            gradle
            jdk11
            stack
            z3
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=$PWD/typechecker/lib:$LD_LIBRARY_PATH
            export GRAAL_HOME="${glittersharkPkgs.graalvm11-ce}"
            export JAVA_HOME="${pkgs.jdk11}/lib/openjdk"
            export GRADLE_HOME="${pkgs.gradle}"
            echo "org.gradle.java.home=$JAVA_HOME" > typechecker/gradle.properties

            $JAVA_HOME/bin/java -version

            $GRAAL_HOME/bin/gu --version
            $GRAAL_HOME/bin/gu list

            $GRADLE_HOME/bin/gradle -version

            stack --version

            z3 --version

            dot2tex --version
          '';
      };
  };
}
