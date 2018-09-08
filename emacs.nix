self: nixpkgs: {
  myEmacsPackageOverrides = super: self: super.melpaPackages // {
    inherit (super) pdf-tools;
    ccls = super.melpaBuild {
      pname = "ccls";
      version = "20180903";
      src = nixpkgs.fetchFromGitHub {
        owner = "MaskRay";
        repo = "emacs-ccls";
        rev = "2b2d5a27ec739b59458e03aa30bb0eb612e727b6";
        sha256 = "018xwfq3kmcp7dpb5cbrcy2cvssywabp6a7qrwnzm3pryvzfrqxd";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/be27a4022d58860917a659fce2b7d7791fbea4e2/recipes/ccls";
        sha256 = "0kiv0n6pdpa75wjcimpwccwbjbhga4gjnphjrkpj4qz5qv42rbnm";
        name = "recipe";
      };
      packageRequires = with super; [dash emacs self.lsp-mode];
      meta = {
        homepage = "https://melpa.org/#/ccls";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-mode = super.melpaBuild {
      pname = "lsp-mode";
      ename = "lsp-mode";
      version = "20180830";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-mode";
        rev = "6eadc0c2a0762b35440a2f6eb6ba27a528334b22";
        sha256 = "0xz5cpmgfgyiakzfa21nqx2xq41kdg4aggahi3qgcn910fp09bi2";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/1a7b69312e688211089a23b75910c05efb507e35/recipes/lsp-mode";
        sha256 = "0cklwllqxzsvs4wvvvsc1pqpmp9w99m8wimpby6v6wlijfg6y1m9";
        name = "recipe";
      };
      packageRequires = with self; [ emacs ];
      meta = {
        homepage = "https://melpa.org/#/lsp-mode";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-ui = super.melpaBuild {
      pname = "lsp-ui";
      ename = "lsp-ui";
      version = "20180904";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-ui";
        rev = "564840a8556ac332cb622f6b743f4471cfabd5dc";
        sha256 = "1a3zfdzfa43b75cypkk6yf4bn9s46mdgajzkswwv005yx0jh5nbr";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/1e4fa7cdf71f49f6998b26d81de9522248bc58e6/recipes/lsp-ui";
        sha256 = "00y5i44yd79z0v00a9lvgixb4mrx9nq5vcgmib70h41ffffaq42j";
        name = "recipe";
      };
      packageRequires = with self; [
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
    # mu4e-conversation = super.melpaBuild {
    #   pname = "mu4e-conversation";
    #   version = "20180711";
    #   src = nixpkgs.fetchgit {
    #     url = https://gitlab.com/ambrevar/mu4e-conversation.git;
    #     rev = "6690d730aaf912b7a2f17caa1e18513c5ee3560e";
    #     sha256 = "1nx9q0d1fk14hfwim456sb1gidmarislf1vlwdkp8mgh5rywxk1q";
    #   };
    #   packageRequires = [];
    #   meta = {
    #     homepage = https://gitlab.com/ambrevar/mu4e-conversation;
    #     license = nixpkgs.lib.licenses.gpl3;
    #   };
    # };
    # lsp-ui = super.melpaBuild {
    #   pname = "lsp-ui";
    #   # version = "20180314.556";
    #   version = "20180618";
    #   src = ~/src/lsp-ui;
    #   packageRequires = with super; [
    #     dash
    #     dash-functional
    #     emacs
    #     flycheck
    #     lsp-mode
    #     markdown-mode
    #   ];
    #   meta = {
    #     homepage = "https://melpa.org/#/lsp-ui";
    #     license = nixpkgs.lib.licenses.free;
    #   };
    # };
    # company-lsp = super.melpaBuild {
    #   pname = "company-lsp";
    #   version = "20180518";
    #   src = nixpkgs.fetchFromGitHub {
    #     owner = "tigersoldier";
    #     repo = "company-lsp";
    #     rev = "88155b0d7cd29f95b0a20c134d28d68ef03c640d";
    #     sha256 = "0fdq3yl10znx6nq50bvxyp87mikapsjv5vj94mprbkw2xib0arhv";
    #   };
    #   packageRequires = with super; [ company dash emacs lsp-mode s ];
    #   meta = {
    #     homepage = "https://melpa.org/#/company-lsp";
    #     license = nixpkgs.lib.licenses.free;
    #   };
    # };
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
      patches = [ ./nix/intero-no-compiler-tools.patch ];
      packageRequires = [ super.company super.flycheck super.haskell-mode ];
      meta = {
        homepage = "https://melpa.org/#/intero";
        license = nixpkgs.lib.licenses.free;
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/1b56ca344ad944e03b669a9974e9b734b5b445bb/recipes/intero";
        sha256 = "15n7ipsq8ylmq4blsycpszkx034j9sb92vqvaz30j5v307fmvs99";
        name = "recipe";
      };
    };
    structured-haskell-mode = super.melpaBuild {
      pname = "shm";
      version = "20170523";
      src = ~/src/structured-haskell-mode;
      packageRequires = [ super.haskell-mode ];
      fileSpecs = [ "elisp/*.el" ];
      propagatedUserEnvPkgs = [ nixpkgs.haskellPackages.structured-haskell-mode ];
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/68a2fddb7e000487f022b3827a7de9808ae73e2a/recipes/shm";
        sha256 = "1qmp8cc83dcz25xbyqd4987i0d8ywvh16wq2wfs4km3ia8a2vi3c";
        name = "recipe";
      };
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
    direnv
    # mu4e-conversation

    # org packages
    orgPackages.org-plus-contrib
    org-bullets
    org-sticky-header
    org-table-sticky-header
    org-journal
    ox-tufte
    ox-gfm
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
    psc-ide
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
    then nixpkgs.emacsPackagesNgGen nixpkgs.emacsMacport
    else nixpkgs.emacsPackagesNg;
  emacs = (self.myEmacsPackagesNg.overrideScope self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
