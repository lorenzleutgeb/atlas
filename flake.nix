{
  description = "atlas";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    examples = {
      url = "github:lorenzleutgeb/atlas-examples";
      flake = false;
    };
    gradle2nix = {
      url = "github:lorenzleutgeb/gradle2nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, gradle2nix, examples }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      jdk = pkgs.jdk17;
      z3 = pkgs.z3.override {
        inherit jdk;
        javaBindings = true;
      };
      graal = pkgs.graalvm17-ce;
      javaToolchains = [ graal ];
      gradle = (pkgs.gradle_7.override { inherit javaToolchains; });
      solvers = with pkgs; [ alt-ergo cvc4 yices opensmt ];
      atlasEnv = pkgs.buildEnv {
        name = "atlas-env";
        paths = [
          gradle
          z3
          pkgs.dot2tex
          pkgs.graphviz
          gradle2nix.packages.${system}.gradle2nix
        ];
      };
      utils = with pkgs; [
        bash
        bash-completion
        coreutils
        gnugrep
        less
        findutils
        nano
      ];
      submission = "CAV2021_paper_294.pdf";
      maintainers = [{
        name = "Lorenz Leutgeb";
        email = "lorenz@leutgeb.xyz";
        github = "lorenzleutgeb";
      }];
    in rec {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [ atlasEnv ];
        shellHook = ''
          export LD_LIBRARY_PATH="${z3.lib}/lib:$LD_LIBRARY_PATH"

          export JAVA_HOME="${jdk}"
          export GRAAL_HOME="${graal}"

          $JAVA_HOME/bin/java -version
          $GRAAL_HOME/bin/gu list

          gradle -version
          z3 --version
          dot2tex --version
        '';
      };

      #defaultPackage.${system} = packages.${system}.atlas;

      packages.${system} = rec {
        atlas-cav = pkgs.fetchurl {
          name = submission;
          url = "https://lorenz.leutgeb.xyz/paper/${submission}";
          sha256 = "sha256-La5oBBs1fyaiUsu6MF1S5yBOYmozXxxsCrldM8EdArU=";
          postFetch = ''
            mkdir $out
            mv -v $downloadedFile $out/${submission}
          '';
          downloadToTemp = true;
          recursiveHash = true;

          meta = {
            inherit maintainers;

            longDescription = ''
              The associated paper as PDF, wrapped in a directory.
            '';
          };
        };

        atlas = (pkgs.callPackage ./gradle-env.nix { }) {
          inherit javaToolchains;
          buildJdk = jdk;

          envSpec = ./gradle-env.json;

          src = ./.;

          nativeBuildInputs =
            [ pkgs.bash pkgs.git pkgs.glibcLocales examples jdk z3 ];
          ATLAS_HOME = "${examples}";
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LD_LIBRARY_PATH = "${z3.lib}/lib";
          gradleFlags = [ "nativeCompile" ];
          outputs = [ "out" ];

          # JaCoCo broken with JDK 17.
          #gradleFlags = [ "jacocoTestReport" "nativeImage" ];
          #outputs = [ "out" "jacoco" ];

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
              build/native/nativeCompile/atlas

            cp -v build/native/nativeCompile/atlas $out/bin/atlas

            # JaCoCo broken with JDK 17.
            #cp -v build/reports/jacoco/test/jacocoTestReport.xml $jacoco

            chmod ug+w $out/var/atlas/atlas.properties
            echo "xyz.leutgeb.lorenz.atlas.module.Loader.defaultHome=$out/var/atlas/resources/examples" >> $out/var/atlas/atlas.properties
          '';

          meta = {
            inherit maintainers;

            longDescription = ''
              The atlas tool, along with some auxiliary files.

              This derivation produces two outputs:
                1. The standard output 'out' contains the binary as
                   `/bin/atlas` and auxiliary files under `/var/atlas`.
                2. The output 'jacoco' which contains a test coverage
                   report in XML format.
                   JaCoCo broken with JDK 17.
            '';
          };
        };

        atlas-shell-docker = pkgs.dockerTools.buildLayeredImage {
          name = "atlas-shell";
          tag = "latest";
          contents = utils ++ [
            packages.${system}.atlas
            packages.${system}.atlas-cav
            packages.${system}.atlas-src
          ];
          config = { Entrypoint = [ "${pkgs.bash}/bin/bash" ]; };

          /* meta = {
               inherit maintainers;

               longDescription = ''
                 A Docker image that contains atlas, alongside some useful tools,
                 the associated paper, and sources.
               '';
             };
          */
        };

        atlas-docker = pkgs.dockerTools.buildLayeredImage {
          name = "atlas";
          tag = "latest";
          contents = [ packages.${system}.atlas ];
          config.Entrypoint = [ (packages.${system}.atlas + "/bin/atlas") ];

          /* meta = {
               inherit maintainers;
               longDescription = ''
                 A Docker image that contains atlas (but not much more),
                 and will run it by default.
               '';
             };
          */
        };

        atlas-ova = nixosConfigurations.atlas.config.system.build.virtualBoxOVA;

        # TODO: Does not work because there's no internet access in sandbox.
        atlas-nix = pkgs.stdenv.mkDerivation {
          name = "atlas-nix";
          src = ./.;
          buildInputs = [ jdk ]; # gradle2nix.packages.${system}.gradle2nix ];
          buildPhase = ''
            gradle2nix --gradle-version 7.0 
          '';
          installPhase = ''
            mkdir $out
            cp gradle-env.* $out
          '';
        };

        atlas-src = pkgs.stdenv.mkDerivation {
          name = "atlas-src";
          src = ./.;
          buildInputs = [ examples ];
          buildPhase = ''
            cp    -Rv    $src        $out

            chmod ug+rwx             $out/src/test/resources

            rm    -rvf               $out/src/test/resources/examples
            ln    -svn   ${examples} $out/src/test/resources/examples
          '';
          installPhase = "true";

          meta = {
            inherit maintainers;
            longDescription = ''
              Takes the contents of this repository and adds
              examples to src/test/resources/examples as if
              it would be initialized and updated as a Git
              submodule.
            '';
          };
        };
      };

      nixosConfigurations.atlas = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          #(nixpkgs + "/nixos/modules/installer/scan/not-detected.nix")
          "${nixpkgs}/nixos/modules/virtualisation/virtualbox-image.nix"
          "${nixpkgs}/nixos/modules/virtualisation/virtualbox-guest.nix"
          home-manager.nixosModules.home-manager
          ({ pkgs, ... }: {
            virtualbox = {
              vmName = "atlas";
              vmFileName = "atlas.ova";
              params = {
                usb = "on";
                usbehci = "off";
                vram = 48;
              };
            };
            environment.systemPackages = utils
              ++ [ self.packages.${system}.atlas pkgs.evince pkgs.vim ];
            networking.hostName = "atlas";
            users.users = {
              evaluator = {
                isNormalUser = true;
                extraGroups = [ "wheel" ];
                uid = 1000;
                shell = pkgs.bash;
                initialHashedPassword = "";
              };
              root.initialHashedPassword = "";
            };
            home-manager.users.evaluator.home = {
              file = let
                referText = ''
                  To prepare evaluation, please open a terminal and execute

                    /home/evaluator/prepare-evaluation.sh

                  This will mount an overlay filesystem at

                    /home/evaluator/atlas

                  Then, please refer to

                    /home/evaluator/atlas/ARTIFACT.md

                  Note that `atlas` (command) is in `$PATH`, it can be executed in a terminal.

                  Note that the associated paper is available at

                    /home/evaluator/${submission}
                '';
                pdf = (self.packages.${system}.atlas-cav + "/" + submission);
              in {
                ${submission}.source = pdf;
                "README.md".text = referText;

                "Desktop/README.md".text = referText;
                "Desktop/${submission}".source = pdf;

                "prepare-evaluation.sh".source =
                  pkgs.writeScript "prepare-evaluation" ''
                    #! ${pkgs.bash}/bin/bash
                    set -xeuo pipefail
                    mkdir --verbose --parents $HOME/.overlay/{upper,work} $HOME/atlas
                    sudo mount --verbose --types overlay overlay --options upperdir=$HOME/.overlay/upper,workdir=$HOME/.overlay/work,lowerdir=${
                      self.packages.${system}.atlas-src
                    } $HOME/atlas
                  '';
              };
              stateVersion = "20.09";
              sessionVariables.ATLAS_HOME = "${examples}";
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
                displayManager = {
                  autoLogin = {
                    enable = true;
                    user = "evaluator";
                  };
                  defaultSession = "xfce";
                };
              };
            };
          })
        ];
      };
    };
}
