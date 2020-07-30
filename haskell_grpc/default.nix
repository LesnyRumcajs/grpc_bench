
{ mkDerivation, base, lens, stdenv }:
mkDerivation {
  pname = "example";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
