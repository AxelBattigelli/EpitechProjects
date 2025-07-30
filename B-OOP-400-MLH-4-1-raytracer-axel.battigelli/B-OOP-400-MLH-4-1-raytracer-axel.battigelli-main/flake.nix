{
  description = "Raytracer Project";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.systems.url = "github:nix-systems/default";
  inputs.flake-utils = {
    url = "github:numtide/flake-utils";
    inputs.systems.follows = "systems";
  };
  inputs.treefmt-nix = {
    url = "github:numtide/treefmt-nix";
    inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      treefmt-nix,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        treefmtEval = treefmt-nix.lib.evalModule pkgs ./treefmt.nix;
        src = ./.;
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            clang-tools
            cmake
            feh
            gdb
            libconfig
            man-pages
            man-pages-posix
            ninja
            pkg-config
            sfml_2
            stdmanpages
            valgrind
          ];
        };
        # for `nix fmt`
        formatter = treefmtEval.config.build.wrapper;
        # for `nix flake check`
        checks = {
          formatting = treefmtEval.config.build.check self;
        };
        packages.default = pkgs.stdenv.mkDerivation {
          pname = "raytracer";
          version = "0.1.0";
          inherit src;

          nativeBuildInputs = with pkgs; [
            cmake
            ninja
            pkg-config
          ];

          buildInputs = with pkgs; [
            sfml_2
            libconfig
          ];

          cmakeFlags = [
            "-DCMAKE_BUILD_TYPE=Release"
          ];
        };
      }
    );
}
