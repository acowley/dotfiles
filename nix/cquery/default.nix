{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2017-12-12";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "f8813c612e5cf243338e5a9726f8638a35654f69";
    sha256 = "10g014s8g82fqkizhsb4dh0kyiz3vvzhj4vwhvpwxiibgvi9snaj";
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

  meta = {
    description = "Low-latency language server for C++, powered by libclang";
    homepage = https://github.com/jacobdufault/cquery;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
