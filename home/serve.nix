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
in
{
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
  ] ++ [stable.cloudcompare];
  home.sessionPath = [
    # Path where uv installs tools
    "$HOME/.local/bin"
  ];

  targets.genericLinux.enable = true;
  home.sessionVariables = {
    NIX_PATH = "nixpkgs=/home/acowley/src/nixpkgs";
    OLLAMA_API_BASE = "http://kubby.local:11434";
  };

  programs.bash = {
    bashrcExtra = ''
      . ${pkgs.bash-completion}/share/bash-completion/bash_completion
      . /etc/bash_completion.d/bazel-complete.bash
    '';

    sessionVariables = {
      GIT_SSH_COMMAND = "ssh";
    };
  };
}
