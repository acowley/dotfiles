{ config, pkgs, ... }:
let nixGL = fetchTarball {
      url = "https://github.com/guibou/nixGL/archive/c4aa5aa15af5d75e2f614a70063a2d341e8e3461.tar.gz";
      sha256 = "sha256:09p7pvdlf4sh35d855lgjk6ciapagrhly9fy8bdiswbylnb3pw5d";
    };
    # myNixGL = (import "${nixGL}/default.nix" {
    #   pkgs = pkgs;
    # }).auto.nixGLNvidia;

    # Technique from 
    # https://github.com/guibou/nixGL/issues/16#issuecomment-903188923
    myNixGL = pkgs.writeShellScriptBin "nixGLNvidia" ''
      $(nix-build ${nixGL} -A auto.nixGLNvidia --no-out-link)/bin/* "$@"
    '';
in
{
  home.packages = with pkgs; [
    google-cloud-sdk
    myNixGL
  ];

  programs.bash = {
    bashrcExtra = ''
      . ${pkgs.bash-completion}/share/bash-completion/bash_completion
    '';
  };
}
