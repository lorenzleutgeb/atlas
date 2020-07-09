{ pkgs ? import <nixpkgs-unstable> {} }:
let deps = [ pkgs.z3 ]; in
pkgs.mkShell {
  buildInputs = deps;
  shellHook = ''
    export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath deps}:$(pwd)/libs:$LD_LIBRARY_PATH"
  '';
}
