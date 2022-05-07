{
  description = "atlas";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    examples = {
      url = "github:lorenzleutgeb/atlas-examples/v0.2.1";
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
        calc
        bash
        bash-completion
        coreutils
        findutils
        gitFull
        gnugrep
        less
        moreutils
        vim
      ];
    in rec {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [ atlasEnv ];
        shellHook = ''
          export LD_LIBRARY_PATH="${z3.lib}/lib:$LD_LIBRARY_PATH"

          export JAVA_HOME="${jdk}"
          export GRAAL_HOME="${graal}"
          export GRADLE_HOME="${gradle}"

          $JAVA_HOME/bin/java -version
          $GRAAL_HOME/bin/gu list

          gradle -version
          z3 --version
          dot2tex --version
        '';
      };

      defaultPackage.${system} = packages.${system}.atlas;

      packages.${system} = rec {
        atlas = (pkgs.callPackage ./gradle-env.nix { gradleBuildJdk = jdk; }) {
          buildJdk = jdk;
          gradlePackage = gradle;

          envSpec = ./gradle-env.json;

          src = ./.;

          nativeBuildInputs =
            [ pkgs.bash pkgs.git pkgs.glibcLocales examples jdk z3 ];
          ATLAS_HOME = "${examples}";
          LANG = "en_US.UTF-8";
          LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
          LD_LIBRARY_PATH = "${z3.lib}/lib";
          gradleFlags = [ "javaToolchains" "nativeCompile" ];
          outputs = [ "out" ];

          # JaCoCo broken with JDK 17.
          #gradleFlags = [ "jacocoTestReport" "nativeImage" ];
          #outputs = [ "out" "jacoco" ];

          configurePhase = ''
            locale
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

          meta.longDescription = ''
            The atlas tool, along with some auxiliary files.

            This derivation produces two outputs:
              1. The standard output 'out' contains the binary as
                 `/bin/atlas` and auxiliary files under `/var/atlas`.
              2. The output 'jacoco' which contains a test coverage
                 report in XML format.
                 JaCoCo broken with JDK 17.
          '';
        };

        atlas-shell-docker = pkgs.dockerTools.buildLayeredImage {
          name = "atlas-shell";
          tag = "latest";
          contents = utils ++ [
            atlasEnv
            packages.${system}.atlas
            packages.${system}.atlas-cav
            packages.${system}.atlas-src
          ];
          config = { Entrypoint = [ "${pkgs.bash}/bin/bash" ]; };

          /* meta = {
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
               longDescription = ''
                 A Docker image that contains atlas (but not much more),
                 and will run it by default.
               '';
             };
          */
        };

        atlas-ova = nixosConfigurations.atlas.config.system.build.virtualBoxOVA;

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

          meta.longDescription = ''
            Takes the contents of this repository and adds
            examples to src/test/resources/examples as if
            it would be initialized and updated as a Git
            submodule.
          '';
        };

        atlas-src-git = pkgs.stdenv.mkDerivation {
          name = "atlas-src-git";
          src = pkgs.fetchFromGitHub {
            owner = "lorenzleutgeb";
            repo = "atlas";
            rev = "8c561d219ba4620f18bed35ead695bcfd2808acb";
            hash = "sha256-aNXuchGIMh27nDIpuLWMlwStA8HKifIxIPNzm/Qx7KI=";
            deepClone = true;
            fetchSubmodules = true;
          };
          buildPhase = ''
            cp    -Rv    $src        $out
          '';
          installPhase = "true";
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
            nix = {
              package = pkgs.nixFlakes;
              extraOptions = ''
                allow-import-from-derivation = true
                experimental-features = nix-command flakes
                keep-outputs = true
              '';
            };
            nixpkgs.config = { allowUnfree = true; };
            virtualbox = {
              vmName = "atlas";
              vmFileName = "atlas.ova";
              params = {
                usb = "on";
                usbehci = "off";
                vram = 48;
              };
            };
            environment = {
              systemPackages = utils
                ++ [ self.packages.${system}.atlas pkgs.evince atlasEnv ];
              variables = {
                "ATLAS_HOME" = "/home/evaluator/atlas/src/resources/examples";
              };
            };
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
            system.activationScripts.atlas.text = ''
              cp -rv ${self.packages.${system}.atlas-src} /home/evaluator/atlas
              rm -rf /home/evaluator/atlas/src/test/resources/examples
              cp -rv ${examples} /home/evaluator/atlas/src/test/resources/examples
              chown -R evaluator:$(id -g evaluator) /home/evaluator/atlas
              chmod -R ug+w /home/evaluator/atlas
            '';
            home-manager.users.evaluator.home = {
              file = let
                referText = ''
                  See

                    /home/evaluator/atlas

                  Please first refer to the `README.md` of the tool itself

                    /home/evaluator/atlas/README.md

                  and afterwards to the "readme" of the artifact

                    /home/evaluator/atlas/ARTIFACT.md

                  Locations of some pre-installed software (all in `$PATH` as applicable):
                    atlas   ${self.packages.${system}.atlas}
                    git     ${pkgs.gitFull}
                    graal   ${graal}
                    gradle  ${gradle}
                    jdk     ${jdk}
                    z3      ${z3}
                '';
              in {
                "README.md".text = referText;
                "Desktop/README.md".text = referText;
              };
              stateVersion = "22.05";
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
