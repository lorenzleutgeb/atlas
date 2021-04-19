{
  description = "atlas";

  inputs = {
    nixpkgs = { url = "nixpkgs/nixos-unstable"; };
    examples = {
      url = "github:lorenzleutgeb/atlas-examples";
      flake = false;
    };
    gradle2nix = {
      url = "github:tadfisher/gradle2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, gradle2nix, examples }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      jdk = pkgs.graalvm11-ce;
      z3 = pkgs.z3.override {
        inherit jdk;
        javaBindings = true;
      };
      gradleGen = pkgs.gradleGen.override { java = jdk; };
      gradle = gradleGen.gradle_latest;
      solvers = with pkgs; [ alt-ergo cvc4 yices opensmt ];
      atlasEnv = pkgs.buildEnv {
        name = "atlas-env";
        paths = [
          gradle
          jdk
          z3
          pkgs.dot2tex
          pkgs.graphviz
          gradle2nix.packages.${system}.gradle2nix
        ];
      };
    in rec {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [ atlasEnv ];
        shellHook = ''
          export LD_LIBRARY_PATH="${z3.lib}/lib:$LD_LIBRARY_PATH"

          export GRAAL_HOME="${jdk}"
          export JAVA_HOME="$GRAAL_HOME"
          export GRADLE_HOME="${gradle}"

          $JAVA_HOME/bin/java -version
          $GRAAL_HOME/bin/gu list
          $GRADLE_HOME/bin/gradle -version
          z3 --version
          dot2tex --version
        '';
      };

      defaultPackage.${system} = packages.${system}.atlas;

      packages.${system} = rec {
        atlas = (pkgs.callPackage ./gradle-env.nix { inherit gradleGen; }) {
          envSpec = ./gradle-env.json;

          src = ./.;

          nativeBuildInputs = [
            pkgs.bash
            pkgs.git
            pkgs.glibcLocales
            examples
            jdk
            z3
          ];
          ATLAS_HOME = "${examples}";
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LD_LIBRARY_PATH = "${z3.lib}/lib";
          gradleFlags = [ "jacocoTestReport" "nativeImage" ];
          outputs = [ "out" "jacoco" ];

          configurePhase = ''
            locale
            patchShebangs version.sh
            rm -rvf src/test/resources/examples
            ln -svn ${examples} src/test/resources/examples
          '';

          installPhase = ''
            mkdir -pv $out/var/atlas/resources $out/bin

            cp -vR ${examples}                     $out/var/atlas/resources/examples
            cp -vR $src/src/test/resources/tactics $out/var/atlas/resources/tactics
            cp -v  $src/atlas.{jsh,properties}     $out/var/atlas

            patchelf \
              --add-needed libz3.so \
              --add-needed libz3java.so \
              --set-rpath ${pkgs.glibc}/lib:${z3.lib}/lib \
              build/native-image/atlas

            cp -v build/native-image/atlas $out/bin/atlas
            cp -v build/reports/jacoco/test/jacocoTestReport.xml $jacoco

            chmod ug+w $out/var/atlas/atlas.properties
            echo "xyz.leutgeb.lorenz.atlas.module.Loader.defaultHome=$out/var/atlas/resources/examples" >> $out/var/atlas/atlas.properties
          '';
        };

        atlas-shell-image = pkgs.dockerTools.buildLayeredImage {
          name = "atlas-shell";
          tag = "latest";
          contents = [
            pkgs.bash
            pkgs.bash-completion
            pkgs.coreutils
            pkgs.gnugrep
            pkgs.less
            packages.${system}.atlas
          ];
          config = { Entrypoint = [ "${pkgs.bash}/bin/bash" ]; };
        };

        atlas-image = pkgs.dockerTools.buildLayeredImage {
          name = "atlas";
          tag = "latest";
          contents = [ packages.${system}.atlas ];
          config.Entrypoint = [ (packages.${system}.atlas + "/bin/atlas") ];
        };

        atlas-ova = nixosConfigurations.atlas.config.system.build.virtualBoxOVA;

        # TODO: Does not work because there's no internet access in sandbox.
        atlas-nix = pkgs.stdenv.mkDerivation {
          name = "atlas-nix";
          src = ./.;
          buildInputs = [ jdk gradle2nix.packages.${system}.gradle2nix ];
          buildPhase = ''
            gradle2nix --gradle-version 7.0 
          '';
          installPhase = ''
            mkdir $out
            cp gradle-env.* $out
          '';
        };
      };

      nixosConfigurations.atlas = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          #(nixpkgs + "/nixos/modules/installer/scan/not-detected.nix")
          "${nixpkgs}/nixos/modules/virtualisation/virtualbox-image.nix"
          "${nixpkgs}/nixos/modules/virtualisation/virtualbox-guest.nix"
          ({ pkgs, ... }: {
            virtualbox = {
              vmName = "atlas";
              params = { usb = "off"; };
            };
            environment.systemPackages = [ self.defaultPackage.${system} ];
            networking.hostName = "atlas";
            users.users.atlas = {
              password = "atlas";
              isNormalUser = true;
              description = "autogenerated user";
              extraGroups = [ "wheel" ];
              uid = 1000;
              shell = pkgs.bash;
            };
            security.sudo.wheelNeedsPassword = false;
            services = {
              openssh.enable = true;
              xserver = {
                enable = true;
                autorun = true;
                desktopManager = {
                  xterm.enable = false;
                  xfce.enable = true;
                };
                displayManager.defaultSession = "xfce";
              };
            };
          })
        ];
      };
    };
}
