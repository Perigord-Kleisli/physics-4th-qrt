{pkgs, ...}: {
  name = "video";
  compiler-nix-name = "ghc902"; # Version of GHC to use
  index-state = "2023-03-01T00:00:00Z";

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.cabal-fmt = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "1.8.0.0";

  shell.buildInputs = with pkgs; [
    nixpkgs-fmt
    libGL
    zlib
    freeglut
    povray
    ffmpeg
    glibc
  ];

  shell.shellHook = ''
    export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:${pkgs.freeglut}/lib"
  '';
}
