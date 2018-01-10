with (import <nixpkgs> {});
stdenv.mkDerivation {
  name = "SDL_gpu_demo";
  src = ./.;
  buildInputs = [SDL2 SDL2_gpu];
  shellHook = ''
    go() {
      clang src/main.c -I${SDL2.dev}/include/SDL2 -I${SDL2_gpu}/include/SDL2 -lSDL2_gpu -lSDL2
    }
  '';
}
