{ config, pkgs, ... }:
{
  # programs.bash = {
  #   sessionVariables = {
  #     BORG_PASSCOMMAND="pass borg-nixos-hp";
  #     BORG_REPO="raspberrypi.local:/mnt/usbdrive/backups/nixos-hp";
  #   };
  # };

  home.packages = with pkgs; [

    # Having this available everywhere is handy for working with YAML files
    yaml-language-server
  ];

  home.sessionVariables = {
    BORG_PASSCOMMAND="pass borg-nixos-hp";
    BORG_REPO="raspberrypi.local:/mnt/usbdrive/backups/nixos-hp";
  };
}
