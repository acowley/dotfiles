self: nixpkgs: {
  myEmacsPackageOverrides = super: self: super.melpaPackages // {
    cmake-mode = super.melpaPackages.cmake-mode.overrideAttrs (_: {
      patchPhase = ''
        sed '2s/.*/;; Version: 0.0/' -i Auxiliary/cmake-mode.el
      '';
    });
    apropospriate-theme =
      # Note: \x27 is a single quote character. This is a way of
      # escaping those characters within the sed command string.
      super.melpaPackages.apropospriate-theme.overrideAttrs (_: {
        patchPhase = ''
          sed 's/(base00   (if (eq variant \x27light) "#FAFAFA" "#424242"))/(base00   (if (eq variant \x27light) "#FAFAFA" "#212121"))/' -i apropospriate.el
        '';
      });

    intero = super.melpaBuild {
      pname = "intero";
      version = "20180219";
      src = ~/src/intero;
      fileSpecs = [ "elisp/*.el" ];
      packageRequires = [ super.company super.flycheck super.haskell-mode ];
      meta = {
        homepage = "https://melpa.org/#/intero";
        license = nixpkgs.lib.licenses.free;
      };
    };
    structured-haskell-mode = super.melpaBuild {
      pname = "shm";
      version = "20170523";
      src = ~/src/structured-haskell-mode;
      packageRequires = [ super.haskell-mode ];
      fileSpecs = [ "elisp/*.el" ];
      propagatedUserEnvPkgs = [ nixpkgs.haskellPackages.structured-haskell-mode ];
      meta = {
        description = "Structured editing Emacs mode for Haskell";
        license = nixpkgs.lib.licenses.bsd3;
        platforms = nixpkgs.haskellPackages.structured-haskell-mode.meta.platforms;
      };
    };
  };
  myEmacsPackages = epkgs: with epkgs; [
    pdf-tools
    use-package
    diminish
    recentf-remove-sudo-tramp-prefix
    visual-fill-column
    apropospriate-theme
    projectile
    yasnippet
    dashboard
    impatient-mode
    esup

    # org packages
    orgPackages.org-plus-contrib
    org-bullets
    org-sticky-header
    org-table-sticky-header
    ox-tufte
    ob-ipython
    org-noter
    outorg
    outshine
    ox-clip
    org-mime
    org-ref

    #
    olivetti
    company
    company-lsp

    helm
    helm-company
    helm-swoop
    helm-dash
    helm-tramp
    docker-tramp
    helm-projectile
    helm-gtags

    # imenu-anywhere
    god-mode
    spaceline
    multiple-cursors
    buffer-move
    flycheck
    nix-mode

    haskell-mode
    hindent
    dante
    structured-haskell-mode
    intero

    lsp-mode
    lsp-ui

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

    rust-mode
    cargo
    flycheck-rust
    racer

    purescript-mode
    paredit
    yaml-mode
    redprl
    osx-dictionary
    graphviz-dot-mode
    nix-buffer
    toml-mode
    markdown-mode
    smartparens
    logview
    ag
  ];
  emacsNix = ((nixpkgs.emacsPackagesNgGen nixpkgs.emacs25Macport).overrideScope self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
