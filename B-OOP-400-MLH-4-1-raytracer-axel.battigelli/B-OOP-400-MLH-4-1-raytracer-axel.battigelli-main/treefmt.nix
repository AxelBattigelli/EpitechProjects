# treefmt.nix
{ ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";

  programs.clang-format.enable = true;
  programs.cmake-format.enable = true;
  programs.nixfmt.enable = true;
  programs.shellcheck.enable = true;
  programs.shfmt.enable = true;
}
