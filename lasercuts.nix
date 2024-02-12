{ mkDerivation, base, diagrams-cairo, diagrams-contrib
, diagrams-lib, diagrams-svg, lib, mtl, relude
}:
mkDerivation {
  pname = "lasercuts";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    base diagrams-cairo diagrams-contrib diagrams-lib diagrams-svg mtl
    relude
  ];
  executableHaskellDepends = [
    base diagrams-cairo diagrams-contrib diagrams-lib diagrams-svg mtl
    relude
  ];
  doHaddock = false;
  homepage = "https://github.com/bacchanalia/lasercuts";
  description = "pew pew lasers";
  license = lib.licenses.gpl3Plus;
  mainProgram = "compassearrings";
}
