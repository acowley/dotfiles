{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2017-12-18";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "900b28d83753d5327672b92a0797015d732dd480";
    sha256 = "1dk0fgrjjhn3k7i590z8pfq6jiy16ql4gb1ayjcybrzb9qdyy9js";
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
