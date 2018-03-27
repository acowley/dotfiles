{ stdenv, writeTextFile, fetchFromGitHub, cmake, llvmPackages, ncurses }:
let flagToLib = flag:
  let name = builtins.elemAt (stdenv.lib.splitString "_" flag) 1;
  in "lib${name}.a";
in llvmPackages.stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2018-03-26";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "310bb882677086f61745369729378ad09d92abb2";
    sha256 = "174w2n31ay46pw0zvrj2hv3c3acx96viv5bixgfgk12gmjxfa4sv";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [
    llvmPackages.llvm llvmPackages.clang llvmPackages.libclang.out
    llvmPackages.libclang.lib ncurses
  ];

  # We don't have a `FindClang.cmake`, so we explicitly set all the
  # variables it would provide.
  cmakeFlags = [
    "-DSYSTEM_CLANG=ON"
    "-DCLANG_CXX=ON"
    "-DClang_LIBRARY=${llvmPackages.libclang.lib}/lib/libclang.so"
    "-DClang_INCLUDE_DIR=${llvmPackages.libclang.out}/include"
  ] ++ (map (inc: "-D${inc}=${llvmPackages.libclang.out}/include")
            [ "Clang_clangFormat_INCLUDE_DIR"
            "Clang_clangToolingCore_INCLUDE_DIR"
            "Clang_clangRewrite_INCLUDE_DIR"
            "Clang_clangAST_INCLUDE_DIR"
            "Clang_clangLex_INCLUDE_DIR"
            "Clang_clangBasic_INCLUDE_DIR"
  ]) ++ (map (lib: "-D${lib}=${llvmPackages.libclang.out}/lib/${flagToLib lib}")
             [ "Clang_clangLex_LIBRARY"
             "Clang_clangFormat_LIBRARY"
             "Clang_clangToolingCore_LIBRARY"
             "Clang_clangRewrite_LIBRARY"
             "Clang_clangAST_LIBRARY"
             "Clang_clangBasic_LIBRARY"
  ]);

  # This helper is provided to help pass include directories nix
  # communicates in the environment and via wrapper scripts to cquery
  # through a compile_commands.json. Tooling like cquery needs to know
  # what the compiler knows, so we augment the compiler command to
  # explicitly include the headers from the nix environment
  # variable. We also pick out the Libsystem include directory needed
  # on darwin.
  setupHook = writeTextFile {
    name = "setup-hook.sh";
    text = ''
    nix-cflags-include() {
      ($CXX -xc++ -E -v /dev/null) 2>&1 | awk 'BEGIN { incsearch = 0} /^End of search list/ { incsearch = 0 } { if(incsearch) { print $0 }} /^#include </ { incsearch = 1 }' | sed 's/^[[:space:]]*\(.*\)/-isystem \1/' | tr '\n' ' '
    }
    nix-cquery() {
      if [ -f compile_commands.json ]; then
        echo "Adding Nix include directories to the compiler commands"
        local extraincs=$(nix-cflags-include)
        sed "s*/bin/\(clang++\|g++\) */bin/\1 ''${extraincs} *" -i compile_commands.json
      else
        echo "There is no compile_commands.json file to edit!"
        echo "Create one with cmake using:"
        echo "(cd build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .. && mv compile_commands.json ..)"
        read -n 1 -p "Do you want to create a .cquery file instead? [Y/n]: " genCquery
        genCquery=''${genCquery:-y}
        case "$genCquery" in
          y|Y ) nix-cflags-include | tr ' ' '\n' > .cquery && echo $'\nGenerated a new .cquery file' ;;
          * ) echo $'\nNot doing anything!' ;;
        esac
      fi
    }
  '';};

  meta = {
    description = "Low-latency language server for C++, powered by libclang";
    homepage = https://github.com/jacobdufault/cquery;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
