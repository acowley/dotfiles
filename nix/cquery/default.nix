{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2017-11-30";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "80737a17709d7fc7ceeee65dedf19f40ac7b3688";
    sha256 = "1hr67ynfrgx5lgvffsl0mvmrwba64aw66wr4jsp119dm2xycj4kh";
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
