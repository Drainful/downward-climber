with import <nixpkgs> {};
  mkShell rec {
    name = "downward-climber";
    buildInputs = [
      sbcl
			SDL2
			libGL
			hello
			ncurses 
    ];
    LD_LIBRARY_PATH = "/run/opengl-driver/lib:/run/opengl-driver-32/lib" + pkgs.stdenv.lib.makeLibraryPath buildInputs;
  }
