self: nixpkgs: {
  myEmacsPackageOverrides = self: super: super.melpaPackages // {
    inherit (super) pdf-tools;
    inherit (super) vterm;

    ox-reveal = super.ox-reveal.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        ./emacs-overlay/ox-reveal-4.0.patch
      ];
    });

    synosaurus = super.synosaurus.overrideAttrs (old: {
      patches = old.patches or [] ++ [
        ./emacs-overlay/synosaurus-wordnet3.1.patch
      ];
    });

    # literate-calc-mode = super.literate-calc-mode.overrideAttrs (old: {
    #   src = nixpkgs.fetchFromGitHub {
    #     owner = "sulami";
    #     repo = "literate-calc-mode.el";
    #     rev = "09dc77ac03cf1b78df4e935a20a8ff731c5bb764";
    #     sha256 = "0kf3xdaws7mg77z24x7319zwi5qi7rhph52ss3q09ym0ammgpw1k";
    #   };
    # });

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

    # ccls = super.melpaBuild {
    #   pname = "ccls";
    #   version = "20191002";
    #   src = nixpkgs.fetchFromGitHub {
    #     owner = "MaskRay";
    #     repo = "emacs-ccls";
    #     rev = "b1acc336f27d8a3bbc750c2dc3be915a4ac1afea";
    #     sha256 = "1qgfxc5d1hb32ks1fxpx7agpw7dvnkz99wydlflc9fqq75g8v142";
    #   };
    #   recipe = nixpkgs.fetchurl {
    #     url = "https://raw.githubusercontent.com/melpa/melpa/be27a4022d58860917a659fce2b7d7791fbea4e2/recipes/ccls";
    #     sha256 = "0kiv0n6pdpa75wjcimpwccwbjbhga4gjnphjrkpj4qz5qv42rbnm";
    #     name = "recipe";
    #   };
    #   patches = [
    #     (nixpkgs.fetchpatch {
    #       # Replace (require 'lsp) with (require 'lsp-mode)
    #       url = "https://patch-diff.githubusercontent.com/raw/MaskRay/emacs-ccls/pull/81.patch";
    #       sha256 = "08n987bwwi0qkaggc976vdkanj9rgq46vhddswa7rw2vny30vlks";
    #     })
    #   ];
    #   packageRequires = with self; [dash emacs self.lsp-mode ht spinner projectile];
    #   meta = {
    #     homepage = "https://melpa.org/#/ccls";
    #     license = nixpkgs.lib.licenses.free;
    #   };
    # };

    lsp-ui = super.lsp-ui.overrideAttrs (_: {
      # patches = [ ./lsp-ui-window-change-no-hide.patch ];
      patches = [ ./lsp-ui-doc-hide-fix.patch ];
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
        patches = [./god-segment-project.patch];
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
    proof-general
    pdf-tools
    use-package
    diminish
    recentf-remove-sudo-tramp-prefix
    visual-fill-column
    apropospriate-theme
    projectile
    yasnippet
    dashboard
    all-the-icons
    impatient-mode
    # esup
    benchmark-init
    direnv
    dired-du
    dired-rsync
    mu4e-conversation
    ace-window

    literate-calc-mode

    # org packages
    orgPackages.org-plus-contrib
    org-superstar
    org-sticky-header
    org-table-sticky-header
    org-journal
    ox-reveal
    ox-tufte
    ox-gfm
    ob-ipython
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
    biblio biblio-core
    helm-bibtex
    parsebib

    #
    olivetti
    company
    company-box

    helm
    helm-company
    helm-swoop
    helm-dash
    helm-tramp
    docker-tramp
    helm-projectile
    helm-gtags
    helm-org-rifle
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

    rust-mode
    cargo
    flycheck-rust
    racer

    dhall-mode
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
  ];
  myEmacsPackagesFor =
    if (false && nixpkgs.stdenv.isDarwin)
    then nixpkgs.emacsPackagesFor nixpkgs.emacsMacport
    # else nixpkgs.emacsPackagesFor (nixpkgs.emacs.override { inherit (nixpkgs) imagemagick; });
    else nixpkgs.emacsPackagesFor ((nixpkgs.emacs.override { imagemagick = nixpkgs.imagemagickBig; srcRepo = true; }).overrideAttrs (old: rec {
      name = "emacs-${version}${versionModifier}";
      version = "28.0";
      versionModifier = ".50";
      src = nixpkgs.fetchgit {
        url = "git://git.sv.gnu.org/emacs.git";
        # rev = "421eeff243af683bf0b7c6d9181650a1c6900f9b";
        # sha256 = "09ci3cyzl3vbvag8ldyrqvg0av0057b8qnq4j7whgp91w52m3vlp";
        rev = "b2581eea1be1468a15927be00ba2f3f399af33a1";
        sha256 = "05dd2h1cacgl8f4r9zh3i5qwl9njdz9dnjdvhyb14h8313y3iwj9";
      };
      patches = [];
      prePatch = ''
        sed 's/\([[:space:]]*\)\(LC_ALL=C $(RUN_TEMACS) -batch $(BUILD_DETAILS) -l loadup --temacs=dump\)/\1env -i \2/' -i src/Makefile.in
        sed 's/\((tramp-compat-process-running-p "gvfs-fuse-daemon")\)/(tramp-compat-process-running-p ".gvfsd-fuse-wrapped") \1/' -i lisp/net/tramp-gvfs.el
        sed 's/error ("Attempt to shape unibyte text");/;/g' -i src/composite.c
      '';
      buildInputs = (old.buildInputs or []) ++ [nixpkgs.jansson];
      configureFlags = (old.configureFlags or []) ++ ["--with-imagemagick"];
    }));
  # emacs = (self.myEmacsPackagesFor.overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
  myemacsPkgs = (self.emacsPackagesFor self.emacsGit).overrideScope' self.myEmacsPackageOverrides;
  myemacs = ((self.emacsPackagesFor self.emacsGit).overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
