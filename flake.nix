{
  description = "Write You a Haskell: compiler and book development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    rust-overlay.url = "github:oxalica/rust-overlay";
    rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { nixpkgs, rust-overlay, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];
      eachSystem = nixpkgs.lib.genAttrs systems;
    in {
      devShells = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ rust-overlay.overlays.default ]; };
          rust = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
          llvm = pkgs.llvmPackages_22;
        in {
          default = pkgs.mkShell {
            packages = [
              rust llvm.llvm.dev llvm.clang pkgs.boehmgc pkgs.pkg-config
              pkgs.just pkgs.python3 pkgs.git pkgs.curl pkgs.unzip
              pkgs.pandoc pkgs.typst pkgs.poppler-utils pkgs.dprint
            ];
            LLVM_SYS_221_PREFIX = "${llvm.llvm.dev}";
            GC_LIB_DIR = "${pkgs.boehmgc}/lib";
            shellHook = ''
              export PATH="$PWD/.tools/bin:$PATH"
            '';
          };
        });
    };
}
