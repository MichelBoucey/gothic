{
  description = "Gothic - A Haskell Vault KVv2 secret engine client";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc928;

        gothic = haskellPackages.callCabal2nix "gothic" ./. { };

      in
      {
        packages = {
          default = gothic;
          gothic = gothic;
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ gothic ];

          buildInputs = with haskellPackages; [
            cabal-install
            haskell-language-server
            ghcid
          ];

          withHoogle = true;

          shellHook = ''
            echo "Gothic development environment"
            echo "GHC version: ${haskellPackages.ghc.version}"
            echo ""
            echo "Available commands:"
            echo "  cabal build    - Build the project"
            echo "  cabal test     - Run tests"
            echo "  cabal repl     - Start GHCi"
            echo "  ghcid          - Auto-reload on changes"
          '';
        };

        # Support for multiple GHC versions
        packages.ghc-8-10 = pkgs.haskell.packages.ghc8107.callCabal2nix "gothic" ./. { };
        packages.ghc-9-0  = pkgs.haskell.packages.ghc90.callCabal2nix "gothic" ./. { };
        packages.ghc-9-2  = pkgs.haskell.packages.ghc928.callCabal2nix "gothic" ./. { };
        packages.ghc-9-4  = pkgs.haskell.packages.ghc948.callCabal2nix "gothic" ./. { };
        packages.ghc-9-6  = pkgs.haskell.packages.ghc966.callCabal2nix "gothic" ./. { };

        # Development shells for different GHC versions
        devShells.ghc-8-10 = pkgs.haskell.packages.ghc8107.shellFor {
          packages = p: [ (pkgs.haskell.packages.ghc8107.callCabal2nix "gothic" ./. { }) ];
          buildInputs = with pkgs.haskell.packages.ghc8107; [ cabal-install ];
        };

        devShells.ghc-9-0 = pkgs.haskell.packages.ghc90.shellFor {
          packages = p: [ (pkgs.haskell.packages.ghc90.callCabal2nix "gothic" ./. { }) ];
          buildInputs = with pkgs.haskell.packages.ghc90; [ cabal-install ];
        };

        devShells.ghc-9-4 = pkgs.haskell.packages.ghc948.shellFor {
          packages = p: [ (pkgs.haskell.packages.ghc948.callCabal2nix "gothic" ./. { }) ];
          buildInputs = with pkgs.haskell.packages.ghc948; [ cabal-install ];
        };

        devShells.ghc-9-6 = pkgs.haskell.packages.ghc966.shellFor {
          packages = p: [ (pkgs.haskell.packages.ghc966.callCabal2nix "gothic" ./. { }) ];
          buildInputs = with pkgs.haskell.packages.ghc966; [ cabal-install ];
        };
      }
    );
}
