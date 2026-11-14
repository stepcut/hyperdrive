{
  description = "hyperdrive: a high-assurance HTTP server";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      haskellPackages = pkgs: pkgs.haskellPackages.extend (final: prev: {
        hyperdrive-semantics = final.callCabal2nix "hyperdrive-semantics" ./hyperdrive-semantics { };
      });
    in {
      packages = forAllSystems (pkgs: {
        duvet = pkgs.callPackage ./nix/duvet.nix { };
        hyperdrive-semantics = (haskellPackages pkgs).hyperdrive-semantics;
      });

      devShells = forAllSystems (pkgs: {
        default = (haskellPackages pkgs).shellFor {
          packages = p: [ p.hyperdrive-semantics ];
          nativeBuildInputs = [
            pkgs.cabal-install
            self.packages.${pkgs.stdenv.hostPlatform.system}.duvet
          ];
        };
      });
    };
}
