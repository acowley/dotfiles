{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2018-01-05";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "84fbc1660a02d8742d8cbc4c2670526e52f9e2e1";
    sha256 = "0x1cb7m30i2zfyx7hk9mfvw06s72yy0x200nr53fyy602vswnsm1";
    fetchSubmodules = true;
  };
  buildInputs = [ python git llvmPackages.llvm llvmPackages.clang llvmPackages.clang-unwrapped ];

  configurePhase = ''
    ./waf configure --use-system-clang --prefix=$out
  '';

  patches = [ ./clang-on-path.patch ];

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
