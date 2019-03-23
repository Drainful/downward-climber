with import <nixpkgs> {};
{
  downward-climber = stdenv.mkDerivation rec {
    name = "downward-climber";
    buildInputs = [
      sbcl
			SDL2
			libGL
    ];
    LD_LIBRARY_PATH = "/run/opengl-driver/lib:/run/opengl-driver-32/lib" + pkgs.stdenv.lib.makeLibraryPath buildInputs;
  };
}

