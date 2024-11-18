{ config, pkgs, ... }:
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
  ];

  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
    };
  };

  home.sessionVariables = {
    BORG_PASSCOMMAND="pass borg-nixos-hp";
    BORG_REPO="raspberrypi.local:/mnt/usbdrive/backups/nixos-hp";
  };
}
