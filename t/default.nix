let
  emacs-overlay = import (builtins.fetchTarball { url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz; });
  emacs-ci = import (builtins.fetchTarball { url = https://github.com/purcell/nix-emacs-ci/archive/master.tar.gz; });

  pkgs = import <nixpkgs> { overlays = [ emacs-overlay ]; };
in
builtins.mapAttrs
  (version: emacs:
    (pkgs.emacsPackagesGen emacs).emacsWithPackages
      (emacsPackages: [
        emacsPackages.dash
        emacsPackages.transient
      ] ++ (with emacsPackages.melpaPackages; [
        libgit
        with-editor
      ])
      ))
  emacs-ci
