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
    nix-cquery() {
      echo "Adding Nix include directories to the compiler commands"
      if [ -f compile_commands.json ]; then
        local extraincs=$(echo $NIX_CFLAGS_COMPILE | awk '{ for(i = 1; i <= NF; i++) if($i == "-isystem") printf "-isystem %s ", $(i+1) }' | sed 's|-isystem \([^[:space:]]*libc++[^[:space:]]*\)|-isystem \1 -isystem \1/c++/v1 |'; cat $(dirname $(which clang++))/../nix-support/libc-cflags | awk '{ for(i = 1; i <= NF; i++) if($i == "-idirafter" && $(i+1) ~ /Libsystem/) printf "-isystem %s", $(i+1) }')
        sed "s|/bin/clang++ |/bin/clang++ ''${extraincs} |" -i compile_commands.json
      else
        echo "There is no compile_commands.json file to edit!"
        echo "Create one with cmake using:"
        echo "(cd build && cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .. && mv compile_commands.json ..)"
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
