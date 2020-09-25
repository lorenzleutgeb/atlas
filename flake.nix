{
  description = "lac";

  inputs.nixpkgs = { url = "nixpkgs/nixos-unstable"; };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in rec {
      devShell."${system}" =
        with pkgs;
        mkShell {
          buildInputs = [
            gradle
            jdk14
            stack
            z3
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=$PWD/typechecker/libs:$LD_LIBRARY_PATH
          '';
      };
  };
}
