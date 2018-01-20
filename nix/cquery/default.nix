{ stdenv, fetchFromGitHub, python, git, llvmPackages }:
stdenv.mkDerivation rec {
  name = "cquery-${version}";
  version = "2018-01-19";

  src = fetchFromGitHub {
    owner = "jacobdufault";
    repo = "cquery";
    rev = "4a328d8afaa7c4fa83abd083b43cc3fff0c2f5aa";
    sha256 = "1sdkh38qby97js3k1pqqhrl8sy57m8fgljhm8gfqhrv190p54sq8";
    fetchSubmodules = true;
  };

  buildInputs = [ python git llvmPackages.clang llvmPackages.clang-unwrapped ];

  configurePhase = ''
    ./waf configure --use-system-clang --prefix=$out --clang-prefix=${llvmPackages.clang-unwrapped} --llvm-config=
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
  setupHook = builtins.toFile "setup-hook.sh" ''
    nix-cflags-include() {
      echo $NIX_CFLAGS_COMPILE | awk '{ for(i = 1; i <= NF; i++) if($i == "-isystem") printf "-isystem %s ", $(i+1) }' | sed 's|-isystem \([^[:space:]]*libc++[^[:space:]]*\)|-isystem \1 -isystem \1/c++/v1|'; cat $(dirname $(which clang++))/../nix-support/libc-cflags | awk '{ for(i = 1; i <= NF; i++) if($i == "-idirafter" && $(i+1) ~ /Libsystem/) printf "-isystem %s", $(i+1) }'
    }
    nix-cquery() {
      if [ -f compile_commands.json ]; then
        echo "Adding Nix include directories to the compiler commands"
        local extraincs=$(nix-cflags-include)
        sed "s|/bin/clang++ |/bin/clang++ ''${extraincs} |" -i compile_commands.json
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
  '';
  meta = {
    description = "Low-latency language server for C++, powered by libclang";
    homepage = https://github.com/jacobdufault/cquery;
    license = stdenv.lib.licenses.mit;
    platforms = stdenv.lib.platforms.all;
  };
}
