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
