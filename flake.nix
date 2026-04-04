{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kast.url = "github:kast-lang/kast";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };
        kast = inputs.kast.packages.${system}.default;
      in with pkgs; {
        devShells.default = mkShell {
          packages = [
            (pkgs.writeShellScriptBin "kast-bootstrap" ''
              systemd-run --quiet --user --scope -p MemoryMax=5G \
                ${kast}/bin/kast "$@"
            '')
            (pkgs.writeShellScriptBin "kast" ''
              # flock --shared target
              node target/kast.mjs "$@"
            '')
            rlwrap
            nixfmt-classic
            nodejs
            just
            fd
            inotify-tools
            hyperfine
          ];
          shellHook = ''
            echo Hello from Kast dev shell
          '';
          KAST_JS_RUNTIME =
            "${inputs.kast.packages.${system}.js-runtime}/runtime.js";
        };
      });
}
