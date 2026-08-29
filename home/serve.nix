{ config, pkgs, nix-gl-host, stable, ... }:
let nixGL = fetchTarball {
      url = "https://github.com/guibou/nixGL/archive/c4aa5aa15af5d75e2f614a70063a2d341e8e3461.tar.gz";
      sha256 = "sha256:09p7pvdlf4sh35d855lgjk6ciapagrhly9fy8bdiswbylnb3pw5d";
    };
    # myNixGL = (import "${nixGL}/default.nix" {
    #   pkgs = pkgs;
    # }).auto.nixGLNvidia;

    # Technique from 
    # https://github.com/guibou/nixGL/issues/16#issuecomment-903188923
    myNixGLNvidia = pkgs.writeShellScriptBin "nixGLNvidia" ''
      $(nix-build ${nixGL} -A auto.nixGLNvidia --no-out-link)/bin/* "$@"
    '';
    myNixGLIntel = pkgs.writeShellScriptBin "nixGLIntel" ''
      $(nix-build ${nixGL} -A nixGLIntel --no-out-link)/bin/* "$@"
    '';
    btop = pkgs.btop.override {
      cudaSupport = true;
    };
    mpv-wrapped = pkgs.writeShellScriptBin "mpv" ''
      exec ${nix-gl-host.defaultPackage.x86_64-linux}/bin/nixglhost ${pkgs.mpv}/bin/mpv "$@"
    '';

    # Fontconfig that picks up standard system fonts + nix fonts
    fontsConf = pkgs.makeFontsConf {
      fontDirectories = [ pkgs.dejavu_fonts pkgs.noto-fonts ];
    };

    # GraalJS jar bundle for the JOSM Scripting Plugin.
    # The nixpkgs graalvmPackages.graaljs is a native standalone (no jars),
    # so we fetch the JVM jar bundle from the plugin's companion repo.
    graaljs-jars = pkgs.fetchzip {
      url = "https://github.com/Gubaer/josm-scripting-plugin-graaljs/releases/download/25.0.2/graaljs-25.0.2.zip";
      stripRoot = false;
      # Replace this with the real hash after the first build attempt
      hash = "sha256-uAIGU4+5Udp54n7m4QTTgFb7o05gQY/s7Vz4Jpy8U00=";
    };

    # JOSM with GraalJS on the module path and working fonts.
    # Uses GraalVM CE as the JDK (has polyglot modules) + GraalJS jars.
    josm-base = pkgs.josm.override { jre = pkgs.graalvmPackages.graalvm-ce; };
    josm-with-graaljs = pkgs.writeShellScriptBin "josm" ''
      export FONTCONFIG_FILE="${fontsConf}"
      export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath [ pkgs.fontconfig pkgs.freetype ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
      export _JAVA_AWT_WM_NONREPARENTING=1
      export _JAVA_OPTIONS="-Dawt.useSystemAAFontSettings=gasp''${_JAVA_OPTIONS:+ $_JAVA_OPTIONS}"
      exec "${pkgs.graalvmPackages.graalvm-ce}/bin/java" \
        --module-path "${graaljs-jars}/lib" \
        --add-modules org.graalvm.polyglot,org.graalvm.word,org.graalvm.collections \
        --add-exports=java.base/sun.security.action=ALL-UNNAMED \
        --add-exports=java.desktop/com.sun.imageio.plugins.jpeg=ALL-UNNAMED \
        --add-exports=java.desktop/com.sun.imageio.spi=ALL-UNNAMED \
        -Djosm.restart=true \
        -Djava.net.useSystemProxies=true \
        -jar "${josm-base}/share/josm/josm.jar" \
        "$@"
    '';

    puppeteerConfig = pkgs.writeText "mmdc-puppeteer-config.json" (builtins.toJSON {
      args = [ "--no-sandbox" "--disable-setuid-sandbox" ];
    });

    mmdc-wrapped = pkgs.symlinkJoin {
      name = "mermaid-cli-wrapped";
      paths = [ pkgs.mermaid-cli ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
      wrapProgram $out/bin/mmdc \
        --add-flags "-p ${puppeteerConfig}"
    '';
    };
in
{
  programs.emacs = pkgs.lib.mkForce {
    enable = true;
    package = pkgs.myemacsPgtk;
  };

  home.packages = with pkgs; [
    google-cloud-sdk
    # kdenlive
    buildifier
    myNixGLNvidia
    myNixGLIntel
    yaml-language-server
    tmux
    emacs-lsp-booster
    (btop.override { cudaSupport = true; })
    nix-gl-host.defaultPackage.x86_64-linux
    uv
    # cloudcompare
    duckdb
    # claude-code
    gh
    mpv-wrapped
    josm-with-graaljs
    mmdc-wrapped
  ] ++ [stable.cloudcompare];
  home.sessionPath = [
    # Path where uv installs tools
    "$HOME/.local/bin"
    "/usr/local/cuda-12.4/bin"
  ];

  targets.genericLinux.enable = true;
  home.sessionVariables = {
    # NIX_PATH = "nixpkgs=/home/acowley/src/nixpkgs";
    OLLAMA_API_BASE = "http://kubby.local:11434";
    UV_KEYRING_PROVIDER = "subprocess";
    UV_INDEX_PRIVATE_REGISTRY_USERNAME = "oauth2accesstoken";
  };

  # Adding to LD_LIBRARY_PATH simplified from
  # https://discourse.nixos.org/t/how-to-extend-environment-variables-in-modules/55492/15
  home.sessionVariablesExtra = ''
    export LD_LIBRARY_PATH="/usr/local/cuda-12.4/lib64''${LD_LIBRARY_PATH:+''${LD_LIBRARY_PATH}}"
  '';

  programs.bash = {
    bashrcExtra = ''
      . ${pkgs.bash-completion}/share/bash-completion/bash_completion
      # . /etc/bash_completion.d/bazel-complete.bash
    '';

    sessionVariables = {
      GIT_SSH_COMMAND = "ssh";
    };
  };

  # programs.mpv = {
  #   enable = true;
  #   # config = {
  #   #   hwdec = "nvdec";
  #   #   keep-open = "always";
  #   # };
  #   # config = {};
  #   # extraConfig = ''
  #   #   hwdec=auto
  #   #   keep-open=always
  #   # '';
  # };
  xdg.configFile."mpv/mpv.conf".text = ''
    hwdec=auto
    keep-open=always
  '';
}
