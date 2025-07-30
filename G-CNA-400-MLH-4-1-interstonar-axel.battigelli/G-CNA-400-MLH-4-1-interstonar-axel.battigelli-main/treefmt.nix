# treefmt.nix
{ ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";

  settings.global.excludes = [
    "vendor/*"
  ];

  programs.nixfmt.enable = true;
  programs.rustfmt.enable = true;
  programs.toml-sort.enable = true;
}
