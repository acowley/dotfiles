{
  allowUnfree = true;
  allowBroken = true;
  packageOverrides = pkgs: rec {
    darwin = pkgs.darwin // {
      # osx_sdk = pkgs.callPackage <nixpkgs/pkgs/os-specific/darwin/osx-sdk/10.11.nix> {};
      osx_sdk = pkgs.callPackage ./nix/osx-sdk/10.11.nix {};
    };
    pass = with pkgs;
           callPackage (<nixpkgs> + /pkgs/tools/security/pass) { x11Support = false; };
    emacs = pkgs.emacs25Macport;
    emacsMacPackagesNg = pkgs.emacsPackagesNgGen emacs;
    irony-server = pkgs.callPackage ./nix/irony-server {};
    mygnused = pkgs.stdenv.mkDerivation {
      name = "mygnused";
      buildInputs = [pkgs.gnused];
      gnused = pkgs.gnused;
      builder = builtins.toFile "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out/bin
        ln -s $gnused/bin/sed $out/bin/gnused
      '';
      meta = {
        description = "GNU sed symlinked to bin/gnused";
        longDescription = ''
          OS X ships with an older version of sed. Rather than shadow
          it in your PATH, this package simply symlinks the gnused
          executable from nixpkgs to the name "gnused".
        '';
      };
    };

    # octave = pkgs.callPackage (<nixpkgs> + /pkgs/development/interpreters/octave) {
    #   openblas = pkgs.openblasCompat;
    #   jdk = null;
    # };
    fltk = pkgs.callPackage ./nix/fltk {};    

    gazebo7 = pkgs.callPackage ./nix/gazebo/7.nix {};
    sdformat4 = pkgs.callPackage (<nixpkgs> + /pkgs/development/libraries/sdformat) { };
    sdformat = sdformat4;
    busybox = null;

    # Uses osx_sdk rather than frameworks from nixpkgs
    glfw = with pkgs; callPackage ./nix/glfw/3.x.nix {};

    # libdevil = (pkgs.libdevil.override {
    #   mesa=null; libX11=null;
    # }).overrideDerivation (_: {
    #     buildInputs = with pkgs;
    #       [ libjpeg libpng libmng lcms1 libtiff openexr ]
    #         ++ (if stdenv.isDarwin
    #             then [darwin.apple_sdk.frameworks.OpenGL]
    #             else [mesa libX11]);
    # });

    # graphviz = (pkgs.graphviz.override {
    #   xorg = null;
    # });

    uriparser = pkgs.uriparser.overrideDerivation (_: {
      configureFlags = ["--disable-doc"];
    });

    platformio = (pkgs.python27.buildEnv.override {
      extraLibs = let p = pkgs.python27Packages;
                  in [ p.setuptools p.pip p.bottle p.platformio ];
    }).env;

    # opencv3 = pkgs.opencv3.override {
    opencv3 = pkgs.callPackage ./nix/opencv/3.x.nix {
      enableContrib = true;
      enableEigen = true;
      enableFfmpeg = true;
      enablePython = true;
    };

    # opencv3 =
    #   let mypy27 = pkgs.python27.buildEnv.override {
    #         extraLibs = [ pkgs.python27Packages.numpy ];
    #       };
    #       mypy3 = pkgs.python3.buildEnv.override {
    #         extraLibs = [ pkgs.python3Packages.numpy ];
    #       };
    #   in (pkgs.opencv3.override {enableContrib = true;}).overrideDerivation
    #   (oldAttrs: {
    #     buildInputs = (with pkgs; [ unzip libjpeg libpng libtiff ])
    #       ++ [ pkgs.eigen pkgs.bzip2 mypy27 mypy3 pkgs.ffmpeg-full ];
    #     nativeBuildInputs = oldAttrs.nativeBuildInputs
    #       ++ [ pkgs.doxygen ]
    #       ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
    #            (with pkgs.darwin.apple_sdk.frameworks;
    #             [ AVFoundation OpenCL QuartzCore QTKit AppKit Cocoa ]);
    #     preConfigure = ''
    #       NIX_CFLAGS_COMPILE=$(echo "$NIX_CFLAGS_COMPILE" | ${pkgs.gnused}/bin/sed "s,[[:space:]]*-F$NIX_STORE/[[:alnum:]]*-CF-osx-[[:digit:].]*/Library/Frameworks,,g")
    #       export PYTHON2_EXECUTABLE=${mypy27}/bin/python
    #       export PYTHON2_LIBRARY=${mypy27}/lib/libpython2.7.dylib
    #       export PYTHON2_INCLUDE_DIR=${mypy27}/include/python2.7
    #       export PYTHON2_PACKAGES_PATH=${mypy27}/lib/python2.7/site-packages
    #       export PYTHON3_EXECUTABLE=${mypy3}/bin/python3
    #       export PYTHON3_LIBRARY=${mypy3}/lib/libpython3.5m.dylib
    #       export PYTHON3_INCLUDE_DIR=${mypy3}/include/python3.5
    #       export PYTHON3_PACKAGES_PATH=${mypy3}/lib/python3.5/site-packages
    #       export EIGEN_ROOT=${pkgs.eigen}
    #       ${pkgs.gnused}/bin/sed -i 's,\(^[[:space:]]*PATH_SUFFIXES include/eigen3 include/eigen2 Eigen/include/eigen3 Eigen/include/eigen2\),\1 eigen3,' ./cmake/OpenCVFindLibsPerf.cmake
    #       ${pkgs.gnused}/bin/sed -i 's|    INSTALL_NAME_DIR lib|    INSTALL_NAME_DIR ''${CMAKE_INSTALL_NAME_DIR}|' ./cmake/OpenCVModule.cmake
    #     '';
    #     cmakeFlags = oldAttrs.cmakeFlags ++ [ "-DWITH_FFMPEG=ON -DWITH_QT=OFF -DWITH_QUICKTIME=OFF -DWITH_AVFOUNDATION=ON" ];
    #     NIX_CFLAGS_COMPILE="-DDEPLOYMENT_TARGET_MACOSX";
    #     patches = [
    #       (pkgs.fetchpatch {
    #          url = https://github.com/opencv/opencv/commit/a2bda999211e8be9fbc5d40038fdfc9399de31fc.patch;
    #          sha256 = "1dmq6gqsy1rzf57acr6kn4xy03vdmmk9b9kn1s0sv5ck9kgc2rfl";
    #       })];
    #   });

    # ros = (pkgs.callPackage ~/Documents/Projects/Nix/Ros).indigo.perception;

    myPythonEnv = pkgs.python3.buildEnv.override {
      extraLibs = with pkgs.python3Packages; [
        numpy scipy matplotlib networkx jupyter_console notebook
      ];};

    myREnv = pkgs.rWrapper.override {
      packages = with pkgs.rPackages;
        [ ggplot2 reshape2 ];
    };

    myNodeEnv = with pkgs;
      [ nodejs npm2nix ] ++ (with nodePackages; [ grunt-cli ]);
  };
}
