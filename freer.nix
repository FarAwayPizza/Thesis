{ mkDerivation, base, pkgs, fetchgit
, lib
}:

mkDerivation {
  pname = "freer-simple";
  version = "1.2.1.2";
  src = fetchgit {
    url = "https://github.com/georgefst/freer-simple.git";
    sha256 = "sha256-/AnRoCx5IRf9Q8+fLk+Wilo16LNxhRxYvCLkuBIWIy0=";
    rev = "e1d88c1ee036115ef527bda8c66da997962b3f34";
  };
  isLibrary = true;
  isExecutable = false;
  libraryHaskellDepends = with pkgs.haskellPackages; [
    base natural-transformation transformers-base template-haskell
    QuickCheck tasty tasty-hunit tasty-quickcheck
  ];
  license = lib.licenses.bsd3;
}
