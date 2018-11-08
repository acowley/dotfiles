self: nixpkgs: {
  myEmacsPackageOverrides = self: super: super.melpaPackages // {
    inherit (super) pdf-tools;
    highlight-indent-guides = super.melpaBuild {
        pname = "highlight-indent-guides";
        ename = "highlight-indent-guides";
        version = "20180910";
        src = nixpkgs.fetchFromGitHub {
          owner = "DarthFennec";
          repo = "highlight-indent-guides";
          rev = "e46356487d4b19144af3025cf16f1b1bd174a450";
          sha256 = "1fm13mx7qcwr0jnwknaja4qg92l2yq1f303hx4fjqm609s5vm1hz";
        };
        recipe = nixpkgs.fetchurl {
          url = "https://raw.githubusercontent.com/milkypostman/melpa/c8acca65a5c134d4405900a43b422c4f4e18b586/recipes/highlight-indent-guides";
          sha256 = "00ghp677qgb5clxhdjarfl8ab3mbp6v7yfsldm9bn0s14lyaq5pm";
          name = "recipe";
        };
        packageRequires = with super; [ emacs ];
        meta = {
          homepage = "https://melpa.org/#/highlight-indent-guides";
          license = nixpkgs.lib.licenses.free;
        };
      };
    ccls = super.melpaBuild {
      pname = "ccls";
      version = "20181022";
      src = nixpkgs.fetchFromGitHub {
        owner = "MaskRay";
        repo = "emacs-ccls";
        rev = "7f0ad5d823703dba59bb2da07e0d153f37f7e26a";
        sha256 = "1hp7pzvjgnb85byaw01sxg2c1asdnnz39kfv6c0zfdqbbdn17fbj";
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
      version = "20181024";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-mode";
        rev = "ef2dfbba55f0df643451f2a796f0097a2a231f94";
        sha256 = "0b02lrl51w47dk3csz5fj693mhvgxsmzqdalac83r52ag931iarg";
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
      version = "20181024";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-ui";
        rev = "05d17b31e7fe11ebeb0f14ee25d55c6d4d05186e";
        sha256 = "1nxvn68zmzfjr2y39h1x7f3s06akzvdh77x54yqxzzxj4cz4g7v5";
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
    #   ename = "mu4e-conversation";
    #   version = "20181019.145";
    #   src = nixpkgs.fetchFromGitLab {
    #     owner = "ambrevar";
    #     repo = "mu4e-conversation";
    #     rev = "b81aff0a3ea7122a29df33ecb60ae23e1fe6adac";
    #     sha256 = "1nrfyd7dqs8iyw7jcfxskkkrchc0mcr1bslsx7gnji06k1ysm7gr";
    #   };
    #   recipe = nixpkgs.fetchurl {
    #     url = "https://raw.githubusercontent.com/milkypostman/melpa/7638aecc7a2cd4b1646c6e32fe83e18ef212bbaa/recipes/mu4e-conversation";
    #     sha256 = "16vhjaxjhshw7ch9ihk35r99549xlbmvybwjx0p9mzyqi30dn3s6";
    #     name = "recipe";
    #   };
    #   packageRequires = with self; [ emacs ];
    #   meta = {
    #     homepage = "https://melpa.org/#/mu4e-conversation";
    #     license = nixpkgs.lib.licenses.free;
    #   };
    # };
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
    dired-du
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
    helm-org-rifle

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
    cuda-mode

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
    web-mode
    smartparens
    logview
    ag
    xterm-color
    highlight-indent-guides
  ];
  myEmacsPackagesNg =
    if nixpkgs.stdenv.isDarwin
    then nixpkgs.emacsPackagesNgGen nixpkgs.emacsMacport
    else nixpkgs.emacsPackagesNgGen (nixpkgs.emacs.override { inherit (nixpkgs) imagemagick; });
  emacs = (self.myEmacsPackagesNg.overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
