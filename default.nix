{ pkgs ? import ./pkgs.nix }:
(import pkgs {}).haskellPackages.developPackage {
  root = ./.;
}
