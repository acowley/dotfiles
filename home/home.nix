{ config, pkgs, ... }:
{
  programs.bash = {
    sessionVariables = {
      BORG_PASSCOMMAND="pass borg-nixos-hp";
      BORG_REPO="raspberrypi.local:/mnt/usbdrive/backups/nixos-hp";
    };
  };
}
