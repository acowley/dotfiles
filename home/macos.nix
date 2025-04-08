{unstable}:
{ config, pkgs, nixpkgs, ... }:
{
  # Add a flake named "n" to the registry for use with, e.g., `nix run n#package`
  # https://discourse.nixos.org/t/how-to-make-nix-shell-use-my-home-manager-flake-input/29957/6
  nix.registry = {
    n = {
      from = {
        type = "indirect";
        id = "n";
      };
      flake = nixpkgs;
    };
  };
  home.packages = with pkgs; [
    ffmpeg-full
    # hunspell
    # hunspellDicts.en_US-large
    (hunspellWithDicts [hunspellDicts.en_US-large])
    pinentry_mac
    wget
    bashInteractive
    terminal-notifier
    ledger
    mosh
    tarsnap
    emacs-lsp-booster
    uv
    duckdb
  ];
  home.homeDirectory = "/Users/acowley";
  home.sessionPath = [
    "/opt/homebrew/bin"
    "/Users/acowley/.ghcup/bin"
    "/Users/acowley/.local/bin"
  ];
  # programs.home-manager.path = "/Users/acowley/src/home-manager";

  home.sessionVariables = {
    OLLAMA_API_BASE = "http://kubby.local:11434";
  };

  programs.zsh = {
    enable = true;
    # enableCompletion = true;
    sessionVariables = {
      NIX_PATH = "nixpkgs=/Users/acowley/src/nixpkgs";
      TMPDIR = "/tmp";
      # DICPATH = "/Users/acowley/.nix-profile/share/hunspell";
      EMACS_SOCKET_NAME = "/tmp/emacs501/server";
      # DYLD_LIBRARY_PATH = "${DYLD_LIBRARY_PATH}:/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
      DYLD_LIBRARY_PATH = "/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
    };
    initExtra = ''
      export TMPDIR=/tmp
    '';
  };

  programs.bash = {
    # enable = pkgs.lib.mkForce false;
    sessionVariables = {
      NIX_PATH = "nixpkgs=/Users/acowley/src/nixpkgs";
      TMPDIR = "/tmp";
      # DICPATH = "/Users/acowley/.nix-profile/share/hunspell";
      EMACS_SOCKET_NAME = "/tmp/emacs501/server";
      # DYLD_LIBRARY_PATH = "${DYLD_LIBRARY_PATH}:/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
      DYLD_LIBRARY_PATH = "/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
    };

    bashrcExtra = ''
      export TMPDIR=/tmp
      . ${pkgs.bash-completion}/share/bash-completion/bash_completion
      for completion_script in ~/.nix-profile/share/bash-completion/completions/*
      do
        source "''${completion_script}"
      done
      if [ -f /opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh ]; then
        source /opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh
      fi
    '';

    initExtra = pkgs.lib.mkOrder 1501 ''
      if [[ :$SHELLOPTS: =~ :(vi|emacs): ]]; then
        source "${pkgs.bash-preexec}/share/bash/bash-preexec.sh"
        eval "$(${unstable.atuin}/bin/atuin init bash)"
      fi
    '';
  };

  home.file.".emacs".source = config.lib.file.mkOutOfStoreSymlink /Users/acowley/dotfiles/dotEmacs;
  home.file.".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink /Users/acowley/dotfiles/early-init.el;
}
