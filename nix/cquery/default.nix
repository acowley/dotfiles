{ stdenv, fetchFromGitHub, writeTextFile, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2018-03-13";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "49ac2e9832f93519b66f6609195225a2ee4c0580";
    sha256 = "18y2sp4ggbjfhmhl7aqrbai8yjw05xd7nbcpvvgikfqwhgi66c86";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ python git ];
  buildInputs = [ llvmPackages.llvm llvmPackages.clang ];

  configurePhase = ''
    unset CXX
    ./waf configure --prefix=$out --clang-prefix=${llvmPackages.clang-unwrapped}
  '';

  buildPhase = ''
    ./waf build
  '';

  installPhase = ''
    ./waf install
  '';

  # This helper is provided to help pass include directories nix
  # communicates in the environment to cquery through a
  # compile_commands.json. Such a file will record a compiler
  # invocation that might work to build a program, but this may rely
  # on the compiler being a nix-built wrapper that adds include paths
  # gleaned from the environment. Tooling like cquery needs to know
  # what the compiler knows, so we dig through the environment and
  # augment the compiler command to explicitly include the headers
  # from the nix environment variable. We also pick out the Libsystem
  # include directory needed on darwin.
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
