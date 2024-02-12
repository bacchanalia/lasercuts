{ mkDerivation, base, diagrams-cairo, diagrams-contrib
, diagrams-lib, diagrams-svg, lib, mtl
}:
mkDerivation {
  pname = "lasercuts";
  version = "0.1.0.0";
  src = /home/zz/code/lasercuts;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base diagrams-cairo diagrams-contrib diagrams-lib diagrams-svg mtl
  ];
  homepage = "https://github.com/bacchanalia/lasercuts";
  description = "pew pew lasers";
  license = lib.licenses.gpl3Only;
  mainProgram = "compassearrings";
}
