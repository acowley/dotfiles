{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    ffmpeg
    hunspell
    hunspellDicts.en_US-large
    pinentry_mac
    wget
    bashInteractive
  ];
  programs.home-manager.path = "/Users/acowley/src/home-manager";
  programs.bash = {
    sessionVariables = {
      NIX_PATH = "nixpkgs=/Users/acowley/src/nixpkgs";
      TMPDIR = "/tmp";
      DICPATH = "/Users/acowley/.nix-profile/share/hunspell";
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
}
