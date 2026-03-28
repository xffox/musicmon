{ lib, haskellPackages }:
haskellPackages.developPackage {
  root = ../.;
  modifier = (args: args // { doCheck = true; });
}
