{
  description = "lac";

  inputs = {
    nixpkgs = { url = "nixpkgs/nixos-20.09"; };
    gradle2nix = {
      url = "github:tadfisher/gradle2nix";
      flake = false;
    };
  };

  /* This flake has a more recent version of Graal that supports Java 11. */
  inputs.glittershark = { url = "github:glittershark/nixpkgs/graalvm-ce"; };

  outputs = { self, nixpkgs, glittershark, gradle2nix }:
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
            packages."${system}".z3
            gradle2nix
          ];
          shellHook = ''
            export Z3_JAVA=$(nix path-info .#packages.x86_64-linux.z3.java)

            export LD_LIBRARY_PATH=$Z3_JAVA:$LD_LIBRARY_PATH
            export GRAAL_HOME="${glittersharkPkgs.graalvm11-ce}"
            export JAVA_HOME="$GRAAL_HOME"
            export GRADLE_HOME="${pkgs.gradle}"
            echo "org.gradle.java.home=$JAVA_HOME" > gradle.properties

            $JAVA_HOME/bin/java -version
            $GRAAL_HOME/bin/gu --version
            $GRAAL_HOME/bin/gu list
            $GRADLE_HOME/bin/gradle -version

            z3 --version
            dot2tex --version

            if [ "$GITHUB_ACTIONS" = "true" ]
            then
              echo "$PATH" >> $GITHUB_PATH
              env | grep -E "^((GRAAL|GRADLE|JAVA)_HOME|LD_LIBRARY_PATH|Z3_JAVA)=" | tee -a $GITHUB_ENV
            fi
          '';
      };
      packages."${system}" = {
        z3 = (pkgs.z3.override { javaBindings = true; jdk = glittersharkPkgs.graalvm11-ce; }).overrideAttrs(old: rec {
        outputs = old.outputs ++ [ "java" ];
        postInstall = old.postInstall + ''
          mkdir $java
          mv com.microsoft.z3.jar $java
          mv libz3java.so $java
        '';
      });

        defaultPackage = 
        let
          buildGradle = pkgs.callPackage ./gradle-env.nix {};
        in
          buildGradle {
            envSpec = ./gradle-env.json;
            src = ./.;
            gradleFlags = [ "native" "-x" "test" ];
            installPhase = ''
              cp -v build/native/lac-* $out
            '';
          };
        }; 
  };
}
