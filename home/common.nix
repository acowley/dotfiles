{ config, pkgs, ... }:
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "acowley";
  home.homeDirectory = "/home/acowley";

  home.packages = with pkgs; [
    cachix
    imagemagick
    jq
    sqlite
    mylatex
    ripgrep
    xclip
    gnupg
    tree
    libqalculate
    afew

    # fonts
    powerline-fonts
    powerline-symbols
    victor-mono
    yanone-kaffeesatz
    montserrat
    (nerdfonts.override { fonts = [ "Hack" "FiraCode" "VictorMono" ]; })
  ];

  programs.bash = {
    enable = true;
    enableVteIntegration = true;

    sessionVariables = {
      NIX_PATH = "/home/acowley/src/nixpkgs";
      EDITOR = "emacsclient";
      TMPDIR="$XDG_RUNTIME_DIR";
    };

    bashrcExtra = ''
      if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
      fi

      function nb() {
        nix build n#$1 --json | ${pkgs.jq}/bin/jq -r '.[0].outputs.out'
      }
    '';

      # # Let the emacs vterm package communicate with emacs. For example,
      # # changing directory in vterm will give emacs a different starting
      # # directory when opening a file.
      # if [[ "$INSIDE_EMACS" = 'vterm' ]] \
      #     && [[ -n ''${EMACS_VTERM_PATH} ]] \
      #     && [[ -f ''${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh ]]; then
      #       source ''${EMACS_VTERM_PATH}/etc/emacs-vterm-bash.sh
      # fi

    shellAliases = {
      pbcopy = "xclip -selection c";
      pbpaste = "xclip -selection clipboard -o";
      # nb = "nix-build --no-out-link '<nixpkgs>' -A";
      nrepl = "nix repl '<nixpkgs>'";
    };
  };

  fonts.fontconfig.enable = true;

  targets.genericLinux.enable = true;
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

  programs.git = {
    enable = true;
    userEmail = "acowley@gmail.com";
    userName = "Anthony Cowley";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.myemacsGcc;
  };
  home.file.".emacs".source = config.lib.file.mkOutOfStoreSymlink /home/acowley/dotfiles/dotEmacs;

  programs.powerline-go = {
    enable = true;
    newline = true;
    modules = ["cwd" "ssh" "dotenv" "nix-shell" "gitlite" "exit"];
    pathAliases = {
      "\\~/Projects/serve/skel" = "skel";
      "\\~/Projects/serve/x" = "x";
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv = {
      enable = true;
      enableFlakes = true;
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 14400;
    maxCacheTtl = 43200;
  };

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);

    # The default here is $XDG_DATA_HOME/password-store (aka ~/.local/share/password-store)
    # settings = { PASSWORD_STORE_DIR = "$HOME/.password-store"; };
  };

  programs.rofi = {
    enable = true;
    theme = "DarkBlue";
  };

  # programs.afew = {
  #   enable = true;
  # };
  home.file.".config/afew/config".source = config.lib.file.mkOutOfStoreSymlink /home/acowley/dotfiles/afew-config;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.11";
}
