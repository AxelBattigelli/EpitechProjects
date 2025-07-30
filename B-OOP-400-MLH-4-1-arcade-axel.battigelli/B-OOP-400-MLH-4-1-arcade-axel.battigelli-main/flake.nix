{
  description = "Arcade Project";

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
      in
      {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            cmake
            (ncurses.override { mouseSupport = true; })
            SDL2
            SDL2_ttf
            SDL2_image
            libwebp
            libtiff
            man-pages
            man-pages-posix
            stdmanpages
            gdb
            valgrind
            sfml_2
          ];
        };
        # for `nix fmt`
        formatter = treefmtEval.config.build.wrapper;
        # for `nix flake check`
        checks = {
          formatting = treefmtEval.config.build.check self;
        };
      }
    );
}
