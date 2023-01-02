{ config, pkgs, ... }:
{
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
  ];
  home.homeDirectory = "/Users/acowley";
  home.sessionPath = [
    "/opt/homebrew/bin"
  ];
  programs.home-manager.path = "/Users/acowley/src/home-manager";
  programs.bash = {
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
    '';
  };
  home.file.".emacs".source = config.lib.file.mkOutOfStoreSymlink /Users/acowley/dotfiles/dotEmacs;
  home.file.".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink /Users/acowley/dotfiles/early-init.el;
}
