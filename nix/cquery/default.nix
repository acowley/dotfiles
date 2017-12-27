{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2017-12-27";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "183b9626c61652c2053d5cf63062c0eef392177a";
    sha256 = "0gk558b731nrrmzs0vx6bjyhrsxgfl7rc0i4g3xg2m0mdsnnx6i4";
    fetchSubmodules = true;
  };
  buildInputs = [ python git llvmPackages.llvm llvmPackages.clang llvmPackages.clang-unwrapped ];

  configurePhase = ''
    ./waf configure --use-system-clang --prefix=$out
  '';

  buildPhase = ''
    ./waf build
  '';

  installPhase = ''
    ./waf install
    mkdir -p $out/share/emacs/site-lisp
    cp emacs/cquery.el $out/share/emacs/site-lisp
  '';

  meta = {
    description = "Low-latency language server for C++, powered by libclang";
    homepage = https://github.com/jacobdufault/cquery;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
