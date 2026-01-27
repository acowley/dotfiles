{ config, pkgs, unstable, ... }:
let
  btop = pkgs.btop.override {
    cudaSupport = true;
  };

  # We move temporary files to ~/Downloads because the Firefox snap
  # can access that directory. This is used to view HTML emails from
  # notmuch in emacs.
  firefox-delayed = pkgs.writeShellScriptBin "firefox-delayed" ''
    persistent="$HOME/Downloads/.email-html-$$.html"
    cp "$1" "$persistent"
    firefox "$persistent" &
    (sleep 300 && rm -f "$persistent") &
  '';
in
{
  # programs.bash = {
  #   sessionVariables = {
  #     BORG_PASSCOMMAND="pass borg-nixos-hp";
  #     BORG_REPO="raspberrypi.local:/mnt/usbdrive/backups/nixos-hp";
  #   };
  # };

  programs.emacs = pkgs.lib.mkForce {
    enable = true;
    package = pkgs.myemacsPgtk;
  };

  home.packages = with pkgs; [

    # Having this available everywhere is handy for working with YAML files
    yaml-language-server
    emacs-lsp-booster
    wl-clipboard
    nix-gl-host
    mpv
    lieer
    notmuch
    google-cloud-sdk
    # gyroflow
    # davinci-resolve
    # (sunshine.override { cudaSupport = true; })
    cloudcompare
    ollama-cuda
    uv
    # unstable.goose-cli
    tmux
    byobu
    claude-code-bun
    btop
    firefox-delayed
  ];

  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
      keep-open = "always";
    };
  };

  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  home.sessionVariables = {
    BORG_PASSCOMMAND="pass borg-nixos-hp";
    BORG_REPO="raspberrypi.local:/mnt/usbdrive/backups/nixos-hp";
    OLLAMA_API_BASE = "http://127.0.0.1:11434";
  };

  # Fix an issue where Firefox won't open .shtml files. Emails may
  # involve such files. When one tries to open them with Firefox,
  # Firefox asks how to handle the file and suggests using Firefox
  # itself. When you click okay, the same dialog is presented in a new
  # tab. This website describes the problem and solving it by adding
  # this mime type association to SHTML files in a user ~/.mime.types
  # file that extends the system wide /etc/mime.types file.
  # http://tuxgraphics.org/npa/open-local-shtml-files-in-firefox/
  home.file.".mime.types" = {
    enable = true;
    text = "text/html shtml";
  };
}
