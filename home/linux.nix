{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    xclip
    afew
  ];
  home.homeDirectory = "/home/acowley";
  programs.bash = {
    ## This causes some trouble with atuin integration. The VTE
    ## settings end up in .bashrc after the atuin setup, resulting in
    ## the atuin history database not being updated as expected.
    # enableVteIntegration = true;
    sessionVariables = {
      NIX_PATH = "nixpkgs=/home/acowley/src/nixpkgs";
    };

    shellAliases = {
      pbcopy = "xclip -selection c";
      pbpaste = "xclip -selection clipboard -o";
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 14400;
    maxCacheTtl = 43200;
  };

  programs.rofi = {
    enable = true;
    theme = "DarkBlue";
  };

  xdg = {
    enable = true;
    mime.enable = true;
    # systemDirs.data = [
    #   # These were all set in the default Ubuntu
    #   # "/usr/share/ubuntu"
    #   # "/home/acowley/.local/share/flatpak/exports/share"
    #   # "/var/lib/flatpak/exports/share"
    #   # "/usr/local/share/"
    #   # "/usr/share/"
    #   # "/var/lib/snapd/desktop"

    #   # Help Gnome find home-manager-installed apps
    #   "/home/acowley/.nix-profile/share/applications"
    # ];
  };

  home.file.".emacs".source = config.lib.file.mkOutOfStoreSymlink /home/acowley/dotfiles/dotEmacs;
  home.file.".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink /home/acowley/dotfiles/early-init.el;

  home.file.".config/afew/config".source = config.lib.file.mkOutOfStoreSymlink /home/acowley/dotfiles/afew-config;
}
