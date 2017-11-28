{stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2017-11-27";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "537fc627303c78320f4e658b3e881f60cfef9677";
    sha256 = "1hbf8r8cgjq2sadyracly2yiym56kgzavlnj6y1p477v9wf8fkcc";
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
