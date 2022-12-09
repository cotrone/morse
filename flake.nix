{
  description = "morse";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          morse =
            final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              # This is used by `nix develop .` to open a shell for use with
              # `cabal`, `hlint` and `haskell-language-server`
              shell.tools = {
                cabal = {};
                ghcid = {};
                haskell-language-server = {};
              };
              # Non-Haskell shell tools go here
              # shell.buildInputs = with pkgs; [ ];
              # This adds `js-unknown-ghcjs-cabal` to the shell.
              # shell.crossPlatforms = p: [p.ghcjs];
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      morse-client = (import ./client { inherit pkgs; });

      # This is a function to generate a static client build from a graph json
      static-morse-client = morseGraph: pkgs.stdenv.mkDerivation {
        name = "static-morse-client";
        src = ./client;

        nativeBuildInputs = [ morse-client.package.buildInputs ];
        buildPhase = ''
            ln -s ${morse-client.nodeDependencies}/lib/node_modules ./node_modules
            cp ${morseGraph} src/graph.json
            export HOME=$(pwd)
            npm run build
        '';

        installPhase = ''
            cp -r build $out/
        '';
      };
      flake = pkgs.morse.flake {};
    in flake // {
      lib.static-morse-client = static-morse-client;
    });
}
