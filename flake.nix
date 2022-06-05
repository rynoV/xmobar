{
  inputs = {
    git-ignore-nix.url = "github:hercules-ci/gitignore.nix/master";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils, git-ignore-nix }:
    let
      overlay = final: prev: {
        haskellPackages = prev.haskellPackages.override (old: {
          overrides = prev.lib.composeExtensions (old.overrides or (_: _: { }))
            (hself: hsuper: {
              xmobar = hself.callCabal2nix "xmobar"
                (git-ignore-nix.lib.gitignoreSource ./.) { };
            });
        });
      };
      overlays = [ overlay ];
    in flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system overlays; };
      in rec {
        devShell = pkgs.haskellPackages.shellFor {
          packages = p: [ p.xmobar ];
          buildInputs = with pkgs; [
            xorg.libX11
            xorg.libXrandr
            xorg.libXrender
            xorg.libXScrnSaver
            xorg.libXext
            xorg.libXft
          ];
        };
        defaultPackage = pkgs.haskellPackages.xmobar;
      }) // {
        inherit overlay overlays;
      };
}
