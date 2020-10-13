{
  description = "lac";

  inputs.nixpkgs = { url = "nixpkgs/nixos-20.09"; };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in rec {
      devShell."${system}" =
        with pkgs;
        mkShell {
          buildInputs = [
            dot2tex
            gradle
            jdk14
            stack
            z3
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=$PWD/typechecker/lib:$LD_LIBRARY_PATH
            export JAVA_OPTS="--enable-preview"
            echo "org.gradle.java.home=$JAVA_HOME" >> typechecker/gradle.properties
          '';
      };
  };
}
