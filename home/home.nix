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
    direnv
    nix-direnv
    imagemagick
    jq
    sqlite
    mylatex
    ripgrep
    rofi

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
      NIX_PATH = "/home/acowley/src/nixpkgs";
    };
    bashrcExtra = ''
      source $HOME/.nix-profile/etc/profile.d/nix.sh
    '';
  };

  fonts.fontconfig.enable = true;

  targets.genericLinux.enable = true;
  xdg = {
    enable = true;
    mime.enable = true;
    systemDirs.data = [
      # These were all set in the default Ubuntu
      "/usr/share/ubuntu"
      "/home/acowley/.local/share/flatpak/exports/share"
      "/var/lib/flatpak/exports/share"
      "/usr/local/share/"
      "/usr/share/"
      "/var/lib/snapd/desktop"

      # Help Gnome find home-manager-installed apps
      "/home/acowley/.nix-profile/share/applications"
    ];
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
  home.file.".emacs".source = config.lib.file.mkOutOfStoreSymlink /home/acowley/dotfiles/emacs;

  programs.powerline-go = {
    enable = true;
    newline = true;
    modules = ["cwd" "ssh" "dotenv" "nix-shell" "gitlite" "exit"];
    pathAliases = {
      "\\~/Projects/serve/skel" = "skel";
      "\\~/Projects/serve/x" = "x";
    };
  };

  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 14400;
    maxCacheTtl = 43200;
  };

  programs.afew = {
    enable = true;
    extraConfig = ''
[MailMover]
folders = acowleySA/Inbox
rename = True
acowleySA/Inbox = 'tag:trash':acowleySA/Trash

# The ListMailsFilter creates too many tags under a `list/*`
# hierarchy. Some mailings pack something that looks like a thread
# identifier into the email's `list-id` header, leaving me with many
# tags not associated to any message in Gmail and other clients.
# [ListMailsFilter]

# [FolderNameFilter]
[SentMailsFilter]
sent_tag=sent
[ArchiveSentMailsFilter]
[Filter.1]
query = to:acowley@scalableautonomy.com
tags = +inbox;+SA
message = acowleySA
[Filter.2]
query = to:info@scalableautonomy.com
tags = +inbox;+SAInfo
message = infoSA
[HeaderMatchingFilter.1]
header = List-Id
pattern = emacs-orgmode.gnu.org
tags = +orgmode
[HeaderMatchingFilter.2]
header = List-Id
pattern = (haskell-cafe.haskell.org|ghc-devs.haskell.org|libraries.haskell.org)
tags = +Haskell
[HeaderMatchingFilter.3]
header = List-ID
pattern = ~sircmpwn/sr.ht-discuss
tags = +srht
[HeaderMatchingFilter.4]
header = List-ID
pattern = RadeonOpenCompute/ROCm
tags = +rocm
[HeaderMatchingFilter.5]
header = List-ID
pattern = gfx-rs/naga
tags = +wgpu
[HeaderMatchingFilter.6]
header = From
pattern = (evesham.k12.nj.us|classroom.google.com)
tags = +School;+Owen
[HeaderMatchingFilter.7]
header = From
pattern = groq.com
tags = +groq
[HeaderMatchingFilter.8]
header = From
pattern = serverobotics.com
tags = +serve
[HeaderMatchingFilter.9]
header = To
pattern = (serve-robotics/(skel|x)|@serverobotics.com)
tags = +serve
[InboxFilter]
tags = -new;-important;
    '';
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  # home.stateVersion = "21.11";
}
