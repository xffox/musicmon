{
  haskellPackages,
  haskell,
  stack,
  nixfmt,
  musicmon,
  ...
}:
let
  # matches the resolver
  ghc = haskell.packages.ghc910;
in
(haskellPackages.extend (final: prev: { inherit musicmon; })).shellFor {
  packages = (p: [ p.musicmon ]);
  nativeBuildInputs = [
    stack
    nixfmt
    (ghc.ghcWithPackages (p: [
      p.haskell-language-server
      p.fourmolu
    ]))
  ];
}
