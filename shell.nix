{ pkgs ? import (builtins.fetchGit {
                   url = "https://github.com/nixos/nixpkgs/";
                   # Commit hash for nixos-unstable as of 2021-03-11
                   ref = "refs/heads/master";
                   rev = "a3228bb6e8bdbb9900f30a11fe09006fdabf7b71";
                 }) { overlays = [ (import ./ocaml-overlay.nix) ]; }

, stdenv ? pkgs.overrideCC pkgs.stdenv pkgs.gcc7
}:

with pkgs;

stdenv.mkDerivation {
  name = "icfp21-benchmarks";
  buildInputs = [
                  # Contributed by Reviewer B.
                  ocamlPackages.findlib ocamlPackages.domainslib ocamlPackages.containers ocamlPackages.sexplib
                  dune_2
                ];
}
