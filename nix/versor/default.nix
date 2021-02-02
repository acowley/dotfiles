{ stdenv, lib, fetchFromGitHub, cmake, libGL, freeglut, glew, glfw, withGraphics ? false, examples ? false }:
stdenv.mkDerivation rec {
  name = "versor-${version}";
  version = "20180616";
  src = fetchFromGitHub {
    owner = "wolftype";
    repo = "versor";
    rev = "0d9dae3d716dd595b271a47590d3c43fc89fbbda";
    sha256 = if withGraphics
    then "0qx9k28y35gpqb2r6q7h3hvhlgxvc7n4wndlcfvpvg3dsgnysaqx"
    else "0jym66nk93nai7mszbcm0spxy01lmbnbzgwgb7nksgd09lp24xa4";
    fetchSubmodules = withGraphics;
  };
  nativeBuildInputs = [ cmake ];
  buildInputs = lib.optionals withGraphics [libGL freeglut glew glfw];
  cmakeFlags = [ "-DOpenGL_GL_PREFERENCE=GLVND"
                 ''-DBUILD_GRAPHICS=${if withGraphics then "ON" else "OFF"}''
                 ''-DGLFW_BUILD_EXAMPLES=${if withGraphics then "ON" else "OFF"}''
                 ''-DGFX_USE_GLFW=${if withGraphics then "ON" else "OFF"}''
                 ''-DBUILD_EXAMPLES=${if examples then "ON" else "OFF"}''
               ];
  # preConfigure =
  #   lib.optionalString withGraphics ''
  #     sed 's/BUILD_GRAPHICS OFF/BUILD_GRAPHICS ON/' -i CMakeLists.txt
  #   '' +
  #   lib.optionalString examples ''
  #     sed 's/BUILD_EXAMPLES OFF/BUILD_EXAMPLES ON/' -i CMakeLists.txt
  #   '';
}
