{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2017-12-11";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "089689c62d48079a5e82651da14d4b4091a5b2c7";
    sha256 = "0vc6a9df5ilws1j4b4fhpwbc8rfgmbpf1lg1wk8ly5siaknalpcb";
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
    mkdir -p $out/clang_resource_dir
    ln -s ${llvmPackages.clang-unwrapped}/lib/clang/*/include $out/clang_resource_dir/include
    mkdir -p $out/share/emacs/site-lisp
    cp emacs/cquery.el $out/share/emacs/site-lisp
  '';

  preFixup = ''
    mv $out/bin/app $out/bin/cquery
  '';

  meta = {
    description = "Low-latency language server for C++, powered by libclang";
    homepage = https://github.com/jacobdufault/cquery;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
