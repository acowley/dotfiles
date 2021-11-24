{ config, pkgs, ... }:
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "acowley";

  home.packages = with pkgs; [
    cachix
    imagemagick
    jq
    sqlite
    mylatex
    ripgrep
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

    sessionVariables = {
      EDITOR = "emacsclient";
      TMPDIR="$XDG_RUNTIME_DIR";
    };

    # The nb alias returns the store path of the `out` output of a
    # nixpkgs attribute. If a second argument is given, it is the name
    # of an output /other/ than `out`. E.g. `nb gcc.cc.lib lib` gives
    # the store path of the `lib` output xoof the `gcc.cc.lib`
    # derivation.
    bashrcExtra = ''
      if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then
        source $HOME/.nix-profile/etc/profile.d/nix.sh
      fi

      function nb() {
        nix build n#$1 --json | ${pkgs.jq}/bin/jq -r ".[0].outputs.''${2:-out}"
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
      nrepl = "nix repl '<nixpkgs>'";
    };
  };

  fonts.fontconfig.enable = true;

  programs.git = {
    enable = true;
    userEmail = "acowley@gmail.com";
    userName = "Anthony Cowley";
    extraConfig = {
      init = { defaultBranch = "main"; };
    };
  };

  programs.emacs = {
    enable = true;
    package = pkgs.myemacsGcc;
  };

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

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);

    # The default here is $XDG_DATA_HOME/password-store (aka ~/.local/share/password-store)
    # settings = { PASSWORD_STORE_DIR = "$HOME/.password-store"; };
  };

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
