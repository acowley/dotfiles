{ config, pkgs, ... }:
let no-uuid = drv: font-dir: extension: pkgs.stdenv.mkDerivation {
      name = "my-${drv.name}";
      builder = pkgs.writeText "builder.sh" ''
        source $stdenv/setup
        mkdir -p $out/share/fonts/${font-dir}
        cp ${drv}/share/fonts/${font-dir}/*.${extension} $out/share/fonts/${font-dir}
      '';
    };
in {
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "acowley";

  home.packages = with pkgs; [
    cachix
    imagemagick
    jq
    (sqlite.override { interactive = true; })
    mylatex
    ripgrep
    tree
    libqalculate
    # afew
    nix-output-monitor

    # fonts
    powerline-fonts
    powerline-symbols
    victor-mono
    # (stdenv.mkDerivation {
    #   name = "my-yanone-kaffeesatz-2004";
    #   builder = writeText "builder.sh" ''
    #     source $stdenv/setup
    #     mkdir -p $out/share/fonts/opentype
    #     cp ${pkgs.yanone-kaffeesatz}/share/fonts/opentype/*.otf $out/share/fonts/opentype
    #   '';
    # })
    (no-uuid yanone-kaffeesatz "opentype" "otf")
    montserrat
    (nerdfonts.override { fonts = [ "Hack" "FiraCode" "VictorMono" "DejaVuSansMono" ]; })
    (pkgs.callPackage ./pkgs/poppins.nix {})
    # roboto
    # (stdenv.mkDerivation {
    #   name = "my-roboto-2.138";
    #   builder = writeText "builder.sh" ''
    #     source $stdenv/setup
    #     mkdir -p $out/share/fonts/truetype
    #     cp ${pkgs.roboto}/share/fonts/truetype/*.ttf $out/share/fonts/truetype
    #   '';
    # })
    (no-uuid roboto "truetype" "ttf")
    roboto-mono
    roboto-slab
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
        nix build --no-link n#$1 --json | ${pkgs.jq}/bin/jq -r ".[0].outputs.''${2:-out}"
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
      today = "${pkgs.lib.getBin pkgs.coreutils}/bin/date +'%Y-%m-%d'";
      yesterday = "${pkgs.lib.getBin pkgs.coreutils}/bin/date +'%Y-%m-%d' -d '1 day ago'";
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
      "\\~/Projects/serve/upslam2" = "upslam2";
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  programs.gpg.enable = true;

  programs.password-store = {
    enable = true;
    package = pkgs.pass.withExtensions (exts: [ exts.pass-otp ]);

    # The default here is $XDG_DATA_HOME/password-store (aka ~/.local/share/password-store)
    # settings = { PASSWORD_STORE_DIR = "$HOME/.password-store"; };
  };

  programs.atuin = {
    enable = true;
    enableBashIntegration = false;
    settings = {
      auto_sync = false;
      search_mode = "skim";
      update_check = false;
      filter_mode_shell_up_key_binding = "session";
      show_preview = true;
    };
  };

  programs.fzf = {
    enable = true;
    enableBashIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = true;
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
