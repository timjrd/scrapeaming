{ pkgs ? import ./pkgs.nix }:
with import pkgs {};
(haskellPackages.developPackage {
  root = ./.;
}).overrideAttrs (_:{
  src = builtins.filterSource (path: type: baseNameOf path != "dist") ./.;

  SCRAPEAMING_ROOT     = ".";
  SCRAPEAMING_XVFB     = "${xorg.xorgserver}/bin/Xvfb";
  SCRAPEAMING_CHROMIUM = "${chromium}/bin/chromium";
  SCRAPEAMING_FFPROBE  = "${ffmpeg}/bin/ffprobe";
})
