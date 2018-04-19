{ stdenv, writeTextFile, fetchFromGitHub, cmake, llvmPackages_6, ncurses }:
llvmPackages_6.stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2018-04-17";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "8a8f926081e604d95686a56ceadc3f9c0da94420";
    sha256 = "123ns139rn9jb70c0gyfdgkx3hjmldhw8hmjnfm7gi3bkg6rkd06";
    fetchSubmodules = true;
    # date = 2018-04-17T21:36:47-07:00;
  };

  nativeBuildInputs = [ cmake ];
  buildInputs = [
    llvmPackages_6.llvm llvmPackages_6.clang llvmPackages_6.libclang.out
    llvmPackages_6.libclang.lib llvmPackages_6.libcxx ncurses
  ];

  # We don't have a `FindClang.cmake`, so we explicitly set all the
  # variables it would provide.
  cmakeFlags = [
    "-DSYSTEM_CLANG=ON"
    "-DClang_LIBRARY=${llvmPackages_6.libclang.lib}/lib/libclang.so"
    "-DClang_INCLUDE_DIR=${llvmPackages_6.libclang.out}/include"
  ];

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
