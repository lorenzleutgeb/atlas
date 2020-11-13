{
  description = "lac";

  inputs = {
    nixpkgs = { url = "nixpkgs/nixos-unstable"; };
    examples = {
      url = "github:lorenzleutgeb/lac-examples";
      flake = false;
    };
    gradle2nix = {
      url = "github:tadfisher/gradle2nix";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, gradle2nix, examples }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      gradleGen =
        pkgs.gradleGen.override { java = pkgs.graalvm11-ce; };
      lacEnv = pkgs.buildEnv {
        name = "lac-env";
        paths = with pkgs; [
            dot2tex
            graalvm11-ce
            self.packages."${system}".gradle
            self.packages."${system}".z3
            gradle2nix
          ];
        };
    in rec {
      devShell."${system}" = with pkgs;
        mkShell {
          buildInputs = [ lacEnv ];
          shellHook = ''
            export Z3_JAVA=$(nix path-info .#packages.x86_64-linux.z3.java)

            export LD_LIBRARY_PATH=$Z3_JAVA:$LD_LIBRARY_PATH
            export GRAAL_HOME="${pkgs.graalvm11-ce}"
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

        z3 = (pkgs.z3.override {
          javaBindings = true;
          jdk = pkgs.graalvm11-ce;
        }).overrideAttrs (old: rec {
          outputs = old.outputs ++ [ "java" ];
          postInstall = old.postInstall + ''
            mkdir $java
            mv com.microsoft.z3.jar $java
            mv libz3java.so $java
          '';
        });

        lac = (pkgs.callPackage ./gradle-env.nix { inherit gradleGen; }) {
          envSpec = ./gradle-env.json;
          src = ./.;
          nativeBuildInputs = [
            pkgs.bash
            pkgs.git
            pkgs.graalvm11-ce
            z3
            examples
          ];
          Z3_JAVA = "${z3.java}";
          LD_LIBRARY_PATH = "${z3.java}";
          gradleFlags = [ "nativeImage" "-x" "test" ];
          configurePhase = ''
            patchShebangs version.sh
          '';
          installPhase = ''
            mkdir $out
            mv lac.jsh $out
            mv lac.properties $out
            echo "xyz.leutgeb.lorenz.lac.module.Loader.defaultHome=$out/examples" >> $out/lac.properties
            cp -Rv ${examples} $out/examples
            mv src/test/resources/tactics $out
            mv build/native-image/lac $out/lac
          '';
        };

        docker-lac-shell = pkgs.dockerTools.buildLayeredImage {
          name = "lac-shell";
          tag = "latest";
          contents = [ lacEnv packages."${system}".lac ];
          config = {
            Entrypoint = pkgs.bash + "/bin/bash";
            Env = [ "PATH=${lacEnv}/bin" ];
          };
        };

        docker-lac = pkgs.dockerTools.buildLayeredImage {
          name = "lac";
          tag = "latest";
          config.Entrypoint = packages."${system}".lac + "/lac";
        };
      };
    };
}
