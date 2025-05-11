{ pkgs, haskell-language-server, stack, nixfmt-classic, ... }:
pkgs.mkShell { packages = [ haskell-language-server stack nixfmt-classic ]; }
