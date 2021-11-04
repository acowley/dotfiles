self: nixpkgs: 
let selfPkgs = self; 
    leanSrc = nixpkgs.fetchFromGitHub {
      owner = "leanprover";
      repo = "lean4";
      rev = "52af4e24715ff1ad6618d952b618f35e2c4307f2";
      sha256 = "sha256:1sp0lavr4vh7nqpysq68xrq9i57y6nr5bn4sy5sczpjhz82swmzz";
    };
in {
  myEmacsPackageOverrides = self: super: super.melpaPackages // {
    # inherit (super) pdf-tools;
    inherit (super) vterm;

    # lean4-mode = super.melpaBuild {
    #   pname = "lean4-mode";
    #   version = "1";
    #   # src = selfPkgs.lean4-mode.src;
    #   src = "${leanSrc}/lean4-mode";
    #   packageRequires = with super.melpaPackages; [ dash f flycheck magit-section lsp-mode s ];
    #   recipe = nixpkgs.writeText "recipe" ''
    #     (lean4-mode :repo "leanprover/lean4" :fetcher github :files ("*.el"))
    #   '';
    #   fileSpecs = [ "*.el" ];
    #     # tail -n +2 lean4-input.el > tmp.el
    #     # mv tmp.el lean4-input.el
    #   prePatch = ''
    #     sed "s/(require 'rx)/(require 'rx)\n(require 'dash)/" -i lean4-syntax.el
    #   '';
    # };

    org-roam-ui = super.trivialBuild {
      pname = "org-roam-ui";
      version = "2021-10-12";
      src = nixpkgs.fetchFromGitHub {
        owner = "org-roam";
        repo = "org-roam-ui";
        rev = "50ebd6c39b40b9b22cd34d932fa9aa5cc334ff37";
        hash = "sha256-I5NGREiViMXDqmY7/4juvLMUqsmnPuRBzaV6pi85TJQ=";
      };
      packageRequires = [self.f self.websocket self.org-roam self.simple-httpd];
      postFixup = ''
        cp -r out $out/share/emacs/site-lisp
      '';
    };

    org-clock-reminder = super.trivialBuild {
      pname = "org-clock-reminder";
      version = "2021-10-10";
      src = nixpkgs.fetchFromGitHub {
        owner = "inickey";
        repo = "org-clock-reminder";
        rev = "9f9b88348ffbc6628f2286dcb4c064b520d0a638";
        hash = "sha256-CnOLlgAPOCquOr++wrL+LayOq02NKawAHqut15TAobY=";
      };
      packageRequires = [self.org];
      postFixup = ''
        cp -r icons $out/share/emacs/site-lisp
      '';
    };

    clip2org = super.trivialBuild {
      pname = "clip2org";
      version = "2021-06-11";
      src = nixpkgs.fetchFromGitHub {
        owner = "acowley";
        repo = "clip2org";
        rev = "e80616a98780f37c7cc87baefd38ad2180f8a98f";
        sha256 = "sha256:1h3fbblhdb0hrrk0cl0j8wcf4x0z0wma971ahl79m9g9394yvfps";
      };
    };

    org-marginalia = super.trivialBuild {
      pname = "org-marginalia";
      version = "2021-08-29";
      src = nixpkgs.fetchFromGitHub {
        owner = "nobiot";
        repo = "org-marginalia";
        rev = "ad93331bf715f6843438d046e97fcd6eac61a4e6";
        hash = "sha256-6DTQlmyH88j99QlhO5nszkCkFu05AjoKHbgkJKhax6k=";
      };
    };

    # ox-reveal = super.ox-reveal.overrideAttrs (old: {
    #   patches = old.patches or [] ++ [
    #     ./emacs-overlay/ox-reveal-4.0.patch
    #   ];
    # });

    # orgPackages = super.orgPackages // {
    #   org-plus-contrib = super.orgPackages.org-plus-contrib.overrideAttrs (old: {
    #     # This is the feature/org-fold-universal-core branch using
    #     # text properties rather than overlays to display hidden text
    #     # in org files.
    #     src = nixpkgs.fetchFromGitHub {
    #       owner = "yantar92";
    #       repo = "org";
    #       rev = "81a803180db886137e8d9da6c270413bb3ffb775";
    #       sha256 = "sha256:1nrxa4qc6wvfvhqw935ggw79f9k4a43w8y6lw95ad4f95096ipz3";
    #     };
    #   });
    # };

    # pdf-tools = super.pdf-tools.overrideAttrs (old: {
    #   patches = old.patches or [] ++ [
    #     # https://github.com/politza/pdf-tools/pull/588
    #     (nixpkgs.fetchpatch {
    #       name = "pdf-tools-undefined-function";
    #       url = "https://patch-diff.githubusercontent.com/raw/politza/pdf-tools/pull/588.patch";
    #       sha256 = "1pr2cjf2f6kbcrhdil3l73lmqmj636h7g4l80gnw5gxg3cwmqkrv";
    #     })
    #   ];
    # });

    treemacs = super.treemacs.overrideAttrs (old: {
      propagatedBuildInputs = old.propagatedBuildInputs or [] ++ [nixpkgs.python3];
    });

    benchmark-init = super.benchmark-init.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        (nixpkgs.fetchpatch {
          name = "Add-version-to-define-obsolete-function-alias";
          url = "https://patch-diff.githubusercontent.com/raw/dholm/benchmark-init-el/pull/16.patch";
          sha256 = "1m03pl4a1q04gw3ykhhj9xsyb9lr6vm7mh3qjzlyplms1i30llcm";
        })
      ];
    });

    lsp-mode = super.lsp-mode.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        ./lsp-unicode.patch
      ];
    });

    god-mode = super.god-mode.overrideAttrs (old: {
      src = nixpkgs.fetchFromGitHub {
        owner = "emacsorphanage";
        repo = "god-mode";
        rev = "be58e6d75ccdea00ae28cd8303e7c5682b865d6f";
        sha256 = "07la740w3wp8fzsrkk21nn4chdnwxvnz9m5g434vvwvjm5mc510q";
      };
    });

    synosaurus = super.synosaurus.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        ./synosaurus-wordnet3.1.patch
      ];
    });

    # magit = super.magit.overrideAttrs (old: {
    #   patches = old.patches or [] ++ [
    #     (nixpkgs.fetchpatch {
    #       name = "no-new-frame-on-commit.patch";
    #       url = "https://github.com/magit/magit/commit/035c24055c0e111187d554a0a214d6f0cbfb4bf1.patch";
    #       sha256 = "17294fmx8psq5zrjnczsdnnvyg5qfjkic55x2fzaf1rbby8wbp2v";
    #       revert = true;
    #     })
    #   ];
    # });

    # This package did not work well for me. With the minor mode
    # enabled, I could not move point within a number!
    number-separator = super.trivialBuild {
      pname = "number-separator";
      version = "0.0.1";
      src = nixpkgs.fetchFromGitHub {
        owner = "legalnonsense";
        repo = "number-separator.el";
        rev = "96a39e3cc58c4b67c5ed91b7d8a4367d2cc49ebb";
        sha256 = "sha256:0f0gvji701cwv7rzys2y7hvczzk21idd6wxdlbzmi6490i9vyb6j";
      };
      postPatch = ''
        sed "s/\((require 'font-lock)\)/\1\n(require 'cl-lib)/" -i number-separator.el
      '';
    };

    deft = super.deft.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        ./deft-org-titles.patch
      ];
      buildInputs = old.buildInputs ++ [self.org-roam];
    });

    # org-roam = super.melpaBuild rec {
    #   pname = "org-roam";
    #   version = "1.1.0";
    #   src = nixpkgs.fetchFromGitHub {
    #     owner = "jethrokuan";
    #     repo = "org-roam";
    #     rev = "v${version}";
    #     sha256 = "18ljww204kf1pbgrrnx7bn6177lw1bs3npywbx2k1b5j35c3j8xv";
    #   };
    #   packageRequires = [ self.f self.dash self.async 
    #                       self.emacsql self.emacsql-sqlite ];
    #   recipe = nixpkgs.writeText "recipe" ''
    #     (org-roam :repo "jethrokuan/org-roam" :fetcher github)
    #   '';
    # };

    # ccls = super.ccls.overrideAttrs (old: {
    #   src = nixpkgs.fetchFromGitHub {
    #     owner = "MaskRay";
    #     repo = "emacs-ccls";
    #     rev = "b8e2f4d9b5bed5b5e8b387ac8e43eff723120b80";
    #     sha256 = "1g0m5xnapfl5wjlylam5696p49qwwkdlngmjv858fabhhk9z0lin";
    #   };
    # });

    lsp-ui = super.lsp-ui.overrideAttrs (_: {
      # patches = [ ./lsp-ui-doc-hide-fix.patch ];
    });

    # cmake-mode = super.melpaPackages.cmake-mode.overrideAttrs (_: {
    #   patchPhase = ''
    #     sed '2s/.*/;; Version: 0.0/' -i Auxiliary/cmake-mode.el
    #   '';
    # });

    apropospriate-theme =
      # Note: \x27 is a single quote character. This is a way of
      # escaping those characters within the sed command string.
      super.melpaPackages.apropospriate-theme.overrideAttrs (_: {
        patchPhase = ''
          sed 's/(base00   (if (eq variant \x27light) "#FAFAFA" "#424242"))/(base00   (if (eq variant \x27light) "#FBF8EF" "#212121"))/' -i apropospriate.el
        '';
      });

    doom-modeline =
      super.melpaPackages.doom-modeline.overrideAttrs (_: {
        patches = [./doom-modals-everywhere.patch];
    });

    # structured-haskell-mode = super.melpaBuild {
    #   pname = "shm";
    #   version = "20170523";
    #   src = ~/src/structured-haskell-mode;
    #   packageRequires = [ super.haskell-mode ];
    #   fileSpecs = [ "elisp/*.el" ];
    #   propagatedUserEnvPkgs = [ nixpkgs.haskellPackages.structured-haskell-mode ];
    #   recipe = nixpkgs.fetchurl {
    #     url = "https://raw.githubusercontent.com/milkypostman/melpa/68a2fddb7e000487f022b3827a7de9808ae73e2a/recipes/shm";
    #     sha256 = "1qmp8cc83dcz25xbyqd4987i0d8ywvh16wq2wfs4km3ia8a2vi3c";
    #     name = "recipe";
    #   };
    #   meta = {
    #     description = "Structured editing Emacs mode for Haskell";
    #     license = nixpkgs.lib.licenses.bsd3;
    #     platforms = nixpkgs.haskellPackages.structured-haskell-mode.meta.platforms;
    #   };
    # };
  };
  myEmacsPackages = epkgs: with epkgs; [
    # proof-general
    pdf-tools
    use-package
    diminish
    recentf-remove-sudo-tramp-prefix
    visual-fill-column
    apropospriate-theme
    projectile
    yasnippet
    dashboard
    page-break-lines
    all-the-icons
    impatient-mode
    # esup
    benchmark-init
    direnv
    dired-du
    dired-rsync
    mu4e-conversation
    ace-window
    notmuch
    literate-calc-mode

    # org packages
    # orgPackages.org-plus-contrib
    org
    org-contrib
    org-superstar
    # org-sticky-header
    # org-table-sticky-header
    org-journal
    ox-reveal
    ox-tufte
    ox-gfm
    ob-ipython
    ob-nim
    org-noter
    org-pdftools
    org-noter-pdftools
    outorg
    outshine
    ox-clip
    org-mime
    org-ref
    org-roam
    org-roam-bibtex
    org-books
    org-ql
    org-marginalia
    clip2org
    org-clock-reminder

    deft

    biblio biblio-core
    helm-bibtex
    parsebib
    auctex
    auctex-latexmk

    #
    olivetti
    company
    company-box
    prescient
    company-prescient
    selectrum
    selectrum-prescient
    consult
    consult-flycheck
    consult-dir
    marginalia
    embark
    embark-consult

    helm
    helm-company
    helm-swoop
    helm-dash
    helm-tramp
    docker-tramp
    helm-projectile
    helm-gtags
    helm-org-rifle
    helm-notmuch
    wgrep-helm

    # imenu-anywhere
    god-mode
    spaceline
    moody
    doom-modeline
    minions
    multiple-cursors
    buffer-move
    flycheck
    flycheck-color-mode-line
    nix-mode
    glsl-mode
    ess

    haskell-mode
    # hindent
    # dante
  # ] ++ nixpkgs.lib.optionals (builtins.pathExists ~/src/intero) [
  #   # This is a hacky way of not building these from source on
  #   # machines where we do not expect to use them.
  #   # structured-haskell-mode
  #   intero
  # ] ++ [
    lsp-mode
    lsp-ui
    lsp-haskell
    lsp-treemacs
    lsp-docker
    ccls

    cmake-mode

    clang-format
    mixed-pitch
    magit

    erc-terminal-notifier
    erc-hl-nicks
    ercn
    znc

    twittering-mode
    corral
    avy

    # rust-mode
    rustic
    cargo
    flycheck-rust
    racer

    dhall-mode
    opencl-mode
    purescript-mode
    psc-ide
    paredit
    yaml-mode
    plantuml-mode
    # redprl
    osx-dictionary
    graphviz-dot-mode
    nix-buffer
    toml-mode
    markdown-mode
    web-mode
    smartparens
    logview
    ag
    ripgrep
    xterm-color
    highlight-indent-guides
    vterm
    docker
    dockerfile-mode
    qml-mode
    emojify
    disk-usage
    speed-type
    pomidor
    synosaurus
    restclient
    gif-screencast
    flyspell-correct
    gnuplot

    # lean4-mode
    org-roam-ui
    which-key
    nim-mode

    bazel
  ];
  myemacsPkgs = (self.emacsPackagesFor self.emacsGit).overrideScope' self.myEmacsPackageOverrides;
  myemacs = ((self.emacsPackagesFor self.emacsGit).overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
  
  myemacsGccPkgs = (self.emacsPackagesFor self.emacsGcc).overrideScope' self.myEmacsPackageOverrides;
  myemacsGcc = ((self.emacsPackagesFor self.emacsGcc).overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;

  emacsGccMac = self.emacsGcc.overrideAttrs (old: {
    configureFlags = old.configureFlags or [] ++ ["--with-mac-metal"];
  });
  myemacsGccMac = ((self.emacsPackagesFor self.emacsGccMac).overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
