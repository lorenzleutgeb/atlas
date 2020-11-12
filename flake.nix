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
      gradleGen = pkgs.gradleGen.override { java = glittersharkPkgs.graalvm11-ce; };
    in rec {
      devShell."${system}" =
        with pkgs;
        mkShell {
          buildInputs = [
            dot2tex
            glittersharkPkgs.graalvm11-ce
            packages."${system}".gradle
            packages."${system}".z3
            gradle2nix
          ];
          shellHook = ''
            export Z3_JAVA=$(nix path-info .#packages.x86_64-linux.z3.java)

            export LD_LIBRARY_PATH=$Z3_JAVA:$LD_LIBRARY_PATH
            export GRAAL_HOME="${glittersharkPkgs.graalvm11-ce}"
            export JAVA_HOME="$GRAAL_HOME"
            export GRADLE_HOME="${pkgs.gradle}"

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
      defaultPackage."${system}" = packages."${system}".lac;
      packages."${system}" = rec {
        gradle = gradleGen.gradle_latest;

        z3 = (pkgs.z3.override { javaBindings = true; jdk = glittersharkPkgs.graalvm11-ce; }).overrideAttrs(old: rec {
        outputs = old.outputs ++ [ "java" ];
        postInstall = old.postInstall + ''
          mkdir $java
          mv com.microsoft.z3.jar $java
          mv libz3java.so $java
        '';
      });

        lac = (pkgs.callPackage ./gradle-env.nix {
            inherit gradleGen;
          }) {
            envSpec = ./gradle-env.json;
            src = ./.;
            nativeBuildInputs = [
              pkgs.bash
              pkgs.git
              glittersharkPkgs.graalvm11-ce
              z3
            ];
            Z3_JAVA = "${z3.java}";
            LD_LIBRARY_PATH = "${z3.java}";
            gradleFlags = [ "nativeImage" "-x" "test" ];
            configurePhase = ''
              patchShebangs version.sh
            '';
            installPhase = ''
              echo "OUT IS"
              echo $out
              ls -la build/native-image
              touch $out
              stat $out
              mv build/native-image/lac $out
            '';
          };
        }; 
  };
}
