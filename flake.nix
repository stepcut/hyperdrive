{
  description = "hyperdrive: a high-assurance HTTP server";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      haskellPackages = pkgs: pkgs.haskellPackages.extend (final: prev: {
        # the differential tests run Python's ipaddress module
        hyperdrive-semantics = pkgs.haskell.lib.addTestToolDepends
          (final.callCabal2nix "hyperdrive-semantics" ./hyperdrive-semantics { }) [ pkgs.python3 ];
        hyperdrive-abnf = final.callCabal2nix "hyperdrive-abnf" ./hyperdrive-abnf { };
        hyperdrive-http1 = final.callCabal2nix "hyperdrive-http1" ./hyperdrive-http1 { };
        hyperdrive-server = final.callCabal2nix "hyperdrive-server" ./hyperdrive-server { };
        # not checked: run with `nix develop .#bench -c scripts/bench`
        hyperdrive-bench = final.callCabal2nix "hyperdrive-bench" ./bench { };
      });
    in {
      packages = forAllSystems (pkgs: {
        duvet = pkgs.callPackage ./nix/duvet.nix { };
        hyperdrive-semantics = (haskellPackages pkgs).hyperdrive-semantics;
        hyperdrive-abnf = (haskellPackages pkgs).hyperdrive-abnf;
        hyperdrive-http1 = (haskellPackages pkgs).hyperdrive-http1;
        hyperdrive-server = (haskellPackages pkgs).hyperdrive-server;
        hyperdrive-bench = (haskellPackages pkgs).hyperdrive-bench;
      });

      # `nix flake check` builds the packages (which runs their test
      # suites) and checks requirement coverage. Mutation testing is too
      # slow for every check; run scripts/mutation-test separately.
      checks = forAllSystems (pkgs: {
        hyperdrive-semantics = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-semantics;
        hyperdrive-abnf = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-abnf;
        hyperdrive-http1 = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-http1;
        hyperdrive-server = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-server;

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
          packages = p: [ p.hyperdrive-semantics p.hyperdrive-abnf p.hyperdrive-http1 p.hyperdrive-server ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.python3 # for scripts/mutation-test
            self.packages.${pkgs.stdenv.hostPlatform.system}.duvet
          ];
        };
        # benchmarks against warp: nix develop .#bench -c scripts/bench
        bench = pkgs.mkShell {
          packages = [
            (haskellPackages pkgs).hyperdrive-bench
            pkgs.wrk
            pkgs.util-linux # taskset
            pkgs.python3
          ];
        };
      });
    };
}
