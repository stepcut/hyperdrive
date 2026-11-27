{
  description = "hyperdrive: a high-assurance HTTP server";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # HPACK encodings of real header sets by several implementations, for
  # hyperdrive-http2's tests (MIT licence)
  inputs.hpack-test-case = { url = "github:http2jp/hpack-test-case"; flake = false; };

  outputs = { self, nixpkgs, hpack-test-case }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});

      haskellPackages = pkgs: pkgs.haskellPackages.extend (final: prev: {
        # the differential tests run Python's ipaddress module
        hyperdrive-semantics = pkgs.haskell.lib.addTestToolDepends
          (final.callCabal2nix "hyperdrive-semantics" ./hyperdrive-semantics { }) [ pkgs.python3 ];
        hyperdrive-abnf = final.callCabal2nix "hyperdrive-abnf" ./hyperdrive-abnf { };
        hyperdrive-http1 = final.callCabal2nix "hyperdrive-http1" ./hyperdrive-http1 { };
        # the tests read the hpack-test-case corpus from HPACK_TEST_CASE
        hyperdrive-http2 = (final.callCabal2nix "hyperdrive-http2" ./hyperdrive-http2 { }).overrideAttrs
          (_: { HPACK_TEST_CASE = "${hpack-test-case}"; });
        hyperdrive-server = final.callCabal2nix "hyperdrive-server" ./hyperdrive-server { };
        # not checked: run with `nix develop .#bench -c scripts/bench`
        hyperdrive-bench = final.callCabal2nix "hyperdrive-bench" ./bench { };
      });

      # The benchmark application on ntex (bench/ntex), with one of
      # ntex's reactors. Nothing else refers to it, so only
      # `nix develop .#bench-ntex` builds it, or fetches Rust.
      benchNtex = pkgs: reactor: pkgs.rustPlatform.buildRustPackage {
        pname = "bench-ntex-${reactor}";
        version = "0.1.0";
        src = ./bench/ntex;
        cargoLock.lockFile = ./bench/ntex/Cargo.lock;
        buildFeatures = [ reactor ];
        postInstall = "mv $out/bin/bench-ntex $out/bin/bench-ntex-${reactor}";
      };

      benchTools = pkgs: [
        (haskellPackages pkgs).hyperdrive-bench
        pkgs.wrk
        pkgs.nghttp2 # h2load: scripts/compare-h2
        pkgs.util-linux # taskset
        pkgs.python3
      ];
    in {
      packages = forAllSystems (pkgs: {
        bench-ntex-uring = benchNtex pkgs "uring";
        bench-ntex-polling = benchNtex pkgs "polling";
        duvet = pkgs.callPackage ./nix/duvet.nix { };
        hyperdrive-semantics = (haskellPackages pkgs).hyperdrive-semantics;
        hyperdrive-abnf = (haskellPackages pkgs).hyperdrive-abnf;
        hyperdrive-http1 = (haskellPackages pkgs).hyperdrive-http1;
        hyperdrive-http2 = (haskellPackages pkgs).hyperdrive-http2;
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
        hyperdrive-http2 = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-http2;
        hyperdrive-server = self.packages.${pkgs.stdenv.hostPlatform.system}.hyperdrive-server;

        # hyperdrive-abnf's and hyperdrive-http2's tests read RFCs from
        # their own copies, which must be the text duvet checks citations
        # against.
        abnf-test-data = pkgs.runCommand "abnf-test-data" { } ''
          for f in ${./hyperdrive-abnf/test/data}/rfc*.txt; do
            cmp "$f" ${./.duvet/specifications/www.rfc-editor.org/rfc}/"$(basename "$f")"
          done
          touch $out
        '';
        http2-test-data = pkgs.runCommand "http2-test-data" { } ''
          for f in ${./hyperdrive-http2/test/data}/rfc*.txt; do
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
          packages = p: [ p.hyperdrive-semantics p.hyperdrive-abnf p.hyperdrive-http1 p.hyperdrive-http2 p.hyperdrive-server ];
          nativeBuildInputs = [
            pkgs.cabal-install
            pkgs.python3 # for scripts/mutation-test
            self.packages.${pkgs.stdenv.hostPlatform.system}.duvet
          ];
          HPACK_TEST_CASE = "${hpack-test-case}";
        };
        # the default shell with llhttp's C library, for the differential
        # tests against it: scripts/differential-llhttp
        llhttp = self.devShells.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ pkgs.llhttp ];
        });
        # the default shell with nghttp2's C library, for the differential
        # tests of HPACK against it: scripts/differential-nghttp2
        nghttp2 = self.devShells.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
          buildInputs = old.buildInputs ++ [ pkgs.nghttp2.dev pkgs.nghttp2.lib ];
        });
        # the default shell with h2spec, for conformance over h2c:
        # nix develop .#h2spec -c scripts/h2spec
        h2spec = self.devShells.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (old: {
          nativeBuildInputs = old.nativeBuildInputs ++ [ pkgs.h2spec ];
        });
        # benchmarks against warp: nix develop .#bench -c scripts/bench
        bench = pkgs.mkShell { packages = benchTools pkgs; };
        # the same, and ntex (Rust) with io_uring and with epoll:
        # SERVERS="hyperdrive ntex-uring ntex-polling" nix develop .#bench-ntex -c scripts/bench
        bench-ntex = pkgs.mkShell {
          packages = benchTools pkgs ++ [ (benchNtex pkgs "uring") (benchNtex pkgs "polling") ];
        };
      });
    };
}
