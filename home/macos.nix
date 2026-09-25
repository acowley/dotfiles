{unstable}:
{ config, pkgs, nixpkgs, unstable, ... }:
{
  # Add a flake named "n" to the registry for use with, e.g., `nix run n#package`
  # https://discourse.nixos.org/t/how-to-make-nix-shell-use-my-home-manager-flake-input/29957/6
  nix.registry = {
    n = {
      from = {
        type = "indirect";
        id = "n";
      };
      flake = nixpkgs;
    };
  };
  home.packages = with pkgs; [
    ffmpeg-full
    # hunspell
    # hunspellDicts.en_US-large
    (hunspell.withDicts (ds: [ds.en_US-large]))
    pinentry_mac
    wget
    bashInteractive
    ledger
    mosh
    tarsnap
    emacs-lsp-booster
    # vscodium
    elan
    uv
    duckdb
    # unstable.goose-cli
    claude-code
    omp
    pass-git-helper
  ];
  home.homeDirectory = "/Users/acowley";
  home.sessionPath = [
    "/opt/homebrew/bin"
    "/Users/acowley/.ghcup/bin"
    "/Users/acowley/.local/bin"
  ];
  # programs.home-manager.path = "/Users/acowley/src/home-manager";

  xdg.enable = true;
  home.sessionVariables = {
    OLLAMA_API_BASE = "http://kubby.local:11434";
  };

  programs.zsh = {
    enable = true;
    dotDir = "${config.xdg.configHome}/zsh";
    # enableCompletion = true;
    sessionVariables = {
      NIX_PATH = "nixpkgs=/Users/acowley/src/nixpkgs";
      TMPDIR = pkgs.lib.mkForce "/tmp";
      # DICPATH = "/Users/acowley/.nix-profile/share/hunspell";
      EMACS_SOCKET_NAME = "/tmp/emacs501/server";
      # DYLD_LIBRARY_PATH = "${DYLD_LIBRARY_PATH}:/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
      DYLD_LIBRARY_PATH = "/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
    };

    # Ensure the nix-daemon is running even if macOS updates roll back
    # parts of the old installation:
    # https://github.com/NixOS/nix/issues/3616#issuecomment-2743947492
    initContent = ''
      [[ ! $(command -v nix) && -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]] && source '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
      export TMPDIR=/tmp
    '';
  };

  programs.bash = {
    # enable = pkgs.lib.mkForce false;
    sessionVariables = {
      NIX_PATH = "nixpkgs=/Users/acowley/src/nixpkgs";
      TMPDIR = pkgs.lib.mkForce "/tmp";
      # DICPATH = "/Users/acowley/.nix-profile/share/hunspell";
      EMACS_SOCKET_NAME = "/tmp/emacs501/server";
      # DYLD_LIBRARY_PATH = "${DYLD_LIBRARY_PATH}:/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
      DYLD_LIBRARY_PATH = "/Applications/MATLAB/MATLAB_Runtime/v97/runtime/maci64:MR/v97/sys/os/maci64:/Applications/MATLAB/MATLAB_Runtime/v97/bin/maci64";
    };

    bashrcExtra = ''
      export TMPDIR=/tmp
      . ${pkgs.bash-completion}/share/bash-completion/bash_completion
      for completion_script in ~/.nix-profile/share/bash-completion/completions/*
      do
        source "''${completion_script}"
      done
      if [ -f /opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh ]; then
        source /opt/homebrew/Caskroom/miniconda/base/etc/profile.d/conda.sh
      fi
    '';

    initExtra = pkgs.lib.mkOrder 1501 ''
      if [[ :$SHELLOPTS: =~ :(vi|emacs): ]]; then
        source "${pkgs.bash-preexec}/share/bash/bash-preexec.sh"
        eval "$(${unstable.atuin}/bin/atuin init bash)"
      fi
    '';
  };

  # The atuin daemon refuses to start if a socket from a previous run
  # is left behind (e.g. after an unclean shutdown). Remove it unless
  # the process named in the pid file is still a running atuin (pids
  # get reused across reboots, so liveness alone is not enough).
  launchd.agents.atuin-daemon.config.ProgramArguments = pkgs.lib.mkForce [
    "${pkgs.writeShellScript "atuin-daemon-start" ''
      dir="${config.xdg.dataHome}/atuin"
      pid=$(head -n1 "$dir/atuin-daemon.pid" 2>/dev/null)
      if [ -S "$dir/daemon.sock" ] && ! ps -p "''${pid:-0}" -o comm= 2>/dev/null | grep -q atuin; then
        rm -f "$dir/daemon.sock"
      fi
      exec ${pkgs.lib.getExe config.programs.atuin.package} daemon start
    ''}"
  ];

  home.file.".emacs".source = config.lib.file.mkOutOfStoreSymlink /Users/acowley/dotfiles/dotEmacs;
  home.file.".emacs.d/early-init.el".source = config.lib.file.mkOutOfStoreSymlink /Users/acowley/dotfiles/early-init.el;
}
