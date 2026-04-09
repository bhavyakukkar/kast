{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    kast.url = "github:kast-lang/kast/bootstrap-ocaml";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs:
    inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];
        pkgs = import inputs.nixpkgs { inherit system overlays; };
        kast-bootstrap = inputs.kast.packages.${system}.default;
        filter = inputs.nix-filter.lib;
      in with pkgs; {
        packages = rec {
          kast-js = stdenv.mkDerivation {
            name = "kast-js";
            src = filter {
              root = ./.;
              include = [ ".justfile" "src" "deps" "std" ];
            };
            buildInputs = [ kast-bootstrap just ];
            buildPhase = ''
              KAST_BIN=${kast-bootstrap}/bin/kast just build
            '';
            installPhase = ''
              mkdir $out
              cp target/kast.mjs $out/kast.mjs
            '';
          };
          kast = pkgs.writeShellApplication {
            name = "kast";
            runtimeInputs = [ nodejs ];
            text = ''
              node ${kast-js}/kast.mjs "$@"
            '';
          };
          default = kast;
        };
        devShells.default = mkShell {
          packages = [
            (pkgs.writeShellScriptBin "kast-bootstrap" ''
              systemd-run --quiet --user --scope -p MemoryMax=10G \
                ${kast-bootstrap}/bin/kast "$@"
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
            clang
          ];
          shellHook = ''
            echo Hello from Kast dev shell
          '';
          # Since I dont have cmake or whatever
          CLANGD_FLAGS = "--query-driver=${pkgs.clang}/bin/clang*";
          KAST_JS_RUNTIME =
            "${inputs.kast.packages.${system}.js-runtime}/runtime.js";
        };
      });
}
