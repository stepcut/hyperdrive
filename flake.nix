{
  description = "hyperdrive: a high-assurance HTTP server";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      haskellPackages = pkgs: pkgs.haskellPackages.extend (final: prev: {
        hyperdrive-semantics = final.callCabal2nix "hyperdrive-semantics" ./hyperdrive-semantics { };
        hyperdrive-abnf = final.callCabal2nix "hyperdrive-abnf" ./hyperdrive-abnf { };
      });
    in {
      packages = forAllSystems (pkgs: {
        duvet = pkgs.callPackage ./nix/duvet.nix { };
        hyperdrive-semantics = (haskellPackages pkgs).hyperdrive-semantics;
        hyperdrive-abnf = (haskellPackages pkgs).hyperdrive-abnf;
      });

      # `nix flake check` builds the packages (which runs their test
      # suites) and checks requirement coverage. Mutation testing is too
      # slow for every check; run scripts/mutation-test separately.
      checks = forAllSystems (pkgs: {
        hyperdrive-semantics = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-semantics;
        hyperdrive-abnf = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-abnf;

        # hyperdrive-abnf's tests read RFCs from their own copies, which
        # must be the text duvet checks citations against.
        abnf-test-data = pkgs.runCommand "abnf-test-data" { } ''
          for f in ${./hyperdrive-abnf/test/data}/rfc*.txt; do
            cmp "$f" ${./.duvet/specifications/www.rfc-editor.org/rfc}/"$(basename "$f")"
          done
          touch $out
        '';

        # Fails if any citation does not match the text of its RFC, or if
        # requirement coverage differs from the committed snapshot.
        duvet = pkgs.runCommand "duvet-report" {
          nativeBuildInputs = [ self.packages.${pkgs.stdenv.hostPlatform.system}.duvet ];
        } ''
          cp -r ${self} src
          chmod -R u+w src
          cd src
          duvet report --ci
          touch $out
        '';
      });

      devShells = forAllSystems (pkgs: {
        default = (haskellPackages pkgs).shellFor {
          packages = p: [ p.hyperdrive-semantics p.hyperdrive-abnf ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.python3 # for scripts/mutation-test
            self.packages.${pkgs.stdenv.hostPlatform.system}.duvet
          ];
        };
      });
    };
}
