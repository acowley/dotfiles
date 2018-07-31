self: nixpkgs: {
  myEmacsPackageOverrides = super: self: super.melpaPackages // {
    inherit (super) pdf-tools;
    ccls = super.melpaBuild {
      pname = "ccls";
      version = "20180729";
      src = nixpkgs.fetchFromGitHub {
        owner = "MaskRay";
        repo = "emacs-ccls";
        rev = "d9f35403fb23c35ab069ff42987a1e98d3470db3";
        sha256 = "116ls6hwsh3bjxv4vdasb58qxr0dll8mi4l2byfs9bdpspr9qbn0";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/be27a4022d58860917a659fce2b7d7791fbea4e2/recipes/ccls";
        sha256 = "0kiv0n6pdpa75wjcimpwccwbjbhga4gjnphjrkpj4qz5qv42rbnm";
        name = "recipe";
      };
      packageRequires = with super; [dash emacs lsp-mode];
      meta = {
        homepage = "https://melpa.org/#/ccls";
        license = nixpkgs.lib.licenses.free;
      };
    };
    mu4e-conversation = super.melpaBuild {
      pname = "mu4e-conversation";
      version = "20180711";
      src = nixpkgs.fetchgit {
        url = https://gitlab.com/ambrevar/mu4e-conversation.git;
        rev = "6690d730aaf912b7a2f17caa1e18513c5ee3560e";
        sha256 = "1nx9q0d1fk14hfwim456sb1gidmarislf1vlwdkp8mgh5rywxk1q";
      };
      packageRequires = [];
      meta = {
        homepage = https://gitlab.com/ambrevar/mu4e-conversation;
        license = nixpkgs.lib.licenses.gpl3;
      };
    };
    lsp-ui = super.melpaBuild {
      pname = "lsp-ui";
      # version = "20180314.556";
      version = "20180618";
      src = ~/src/lsp-ui;
      packageRequires = with super; [
        dash
        dash-functional
        emacs
        flycheck
        lsp-mode
        markdown-mode
      ];
      meta = {
        homepage = "https://melpa.org/#/lsp-ui";
        license = nixpkgs.lib.licenses.free;
      };
    };
    company-lsp = super.melpaBuild {
      pname = "company-lsp";
      version = "20180518";
      src = nixpkgs.fetchFromGitHub {
        owner = "tigersoldier";
        repo = "company-lsp";
        rev = "88155b0d7cd29f95b0a20c134d28d68ef03c640d";
        sha256 = "0fdq3yl10znx6nq50bvxyp87mikapsjv5vj94mprbkw2xib0arhv";
      };
      packageRequires = with super; [ company dash emacs lsp-mode s ];
      meta = {
        homepage = "https://melpa.org/#/company-lsp";
        license = nixpkgs.lib.licenses.free;
      };
    };
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
    mu4e-conversation

    # org packages
    orgPackages.org-plus-contrib
    org-bullets
    org-sticky-header
    org-table-sticky-header
    org-journal
    ox-tufte
    ob-ipython
    org-noter
    outorg
    outshine
    ox-clip
    org-mime
    org-ref
    biblio biblio-core
    helm-bibtex
    parsebib

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
    moody
    minions
    multiple-cursors
    buffer-move
    flycheck
    flycheck-color-mode-line
    nix-mode
    glsl-mode

    haskell-mode
    hindent
    dante
    structured-haskell-mode
    intero

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
    xterm-color
  ];
  myEmacsPackagesNg =
    if nixpkgs.stdenv.isDarwin
    then nixpkgs.emacsPackagesNgGen nixpkgs.emacs25Macport
    else nixpkgs.emacsPackagesNg;
  emacs = (self.myEmacsPackagesNg.overrideScope self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
