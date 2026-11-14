# duvet: requirements traceability tool from AWS Labs
# https://github.com/awslabs/duvet
#
# Not packaged in nixpkgs, so we build it from crates.io.
{ lib, rustPlatform, fetchCrate, pkg-config, openssl }:

rustPlatform.buildRustPackage rec {
  pname = "duvet";
  version = "0.4.3";

  src = fetchCrate {
    inherit pname version;
    hash = "sha256-DUKjNzWKi+IbbyWyjaYxgH92HgcNqR4Lx85AlTlohX4=";
  };

  cargoHash = "sha256-bGd1w4q+PqTrvTjcn5+HCX+rjfysdtGJBGzvXr/6tiA=";

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ openssl ];

  # the crate's test suite needs network access and its own fixtures
  doCheck = false;

  meta = {
    description = "Code quality tool to help bound correctness";
    homepage = "https://github.com/awslabs/duvet";
    license = lib.licenses.asl20;
    mainProgram = "duvet";
  };
}
