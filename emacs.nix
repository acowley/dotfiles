self: nixpkgs: {
  myEmacsPackageOverrides = self: super: super.melpaPackages // {
    inherit (super) pdf-tools;
    inherit (super) emacs-libvterm;
    disk-usage = super.elpaBuild {
      pname = "disk-usage";
      ename = "disk-usage";
      version = "20190220";
      src = (nixpkgs.fetchFromGitLab {
        owner = "ambrevar";
        repo = "emacs-disk-usage";
        rev = "7b148294a2807ce770b37bb6a7c54080be459990";
        sha256 = "1fk0bj9n70zsvm07r5c02fqh0sz19kjsjic92764dzz2gva06xq9";
      }) + /disk-usage.el;
      packageRequires = [ super.emacs ];
      meta = {
        homepage = "https://elpa.gnu.org/packages/disk-usage.html";
        license = nixpkgs.lib.licenses.free;
      };
    };
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
      version = "20190414";
      src = nixpkgs.fetchFromGitHub {
        owner = "MaskRay";
        repo = "emacs-ccls";
        rev = "3f7a981658ccd71d2e09b9ad2b31a1038e55dcd4";
        sha256 = "1pc0zchwwjdx8n4p1y3s0w9jv2blq7p2ymm9218nrghjjmj2fxnl";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/be27a4022d58860917a659fce2b7d7791fbea4e2/recipes/ccls";
        sha256 = "0kiv0n6pdpa75wjcimpwccwbjbhga4gjnphjrkpj4qz5qv42rbnm";
        name = "recipe";
      };
      packageRequires = with super; [dash emacs self.lsp-mode ht spinner];
      meta = {
        homepage = "https://melpa.org/#/ccls";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-mode = super.melpaBuild {
      pname = "lsp-mode";
      ename = "lsp-mode";
      version = "20190604";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-mode";
        rev = "a07f703693ceffbf75e61bfa1786a991215bf463";
        sha256 = "1bk0nbfwjk73jlrg4lbgir7rvh9mfd9dxrn776a9p08qfkshhqv0";
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
      version = "20190523";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-ui";
        rev = "3ccc3e3386732c3ee22c151e6b5215a0e4c99173";
        sha256 = "1k51lwrd3qy1d0afszg1i041cm8a3pz4qqdj7561sncy8m0szrwk";
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
    company-lsp = super.melpaBuild {
      pname = "company-lsp";
      ename = "company-lsp";
      version = "20190524";
      src = nixpkgs.fetchFromGitHub {
        owner = "tigersoldier";
        repo = "company-lsp";
        rev = "cd1a41583f2d71baef44604a14ea71f49b280bf0";
        sha256 = "0qxwl5w9l3bab35r9d8g2yfrykj6gzn8jz2b8h2rfkzd36k7zv11";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/13d1a86dfe682f65daf529f9f62dd494fd860be9/recipes/company-lsp";
        sha256 = "09nbi6vxw8l26gfgsc1k3bx4m8i1px1b0jxaywszky5bv4fdy03l";
        name = "recipe";
      };
      packageRequires = with self; [ company dash emacs lsp-mode s ];
      meta = {
        homepage = "https://melpa.org/#/company-lsp";
        license = nixpkgs.lib.licenses.free;
      };
    };
    mu4e-conversation = super.melpaBuild {
      pname = "mu4e-conversation";
      ename = "mu4e-conversation";
      version = "20181203.145";
      src = nixpkgs.fetchFromGitLab {
        owner = "ambrevar";
        repo = "mu4e-conversation";
        rev = "ae5ad4beed8aa69d1400f6f733f84f440ccfc1a7";
        sha256 = "0cli4d3vxqx6j23rnzqx27aby14mdrwpi3aik79a9ig1jgd7kzn7";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/7638aecc7a2cd4b1646c6e32fe83e18ef212bbaa/recipes/mu4e-conversation";
        sha256 = "16vhjaxjhshw7ch9ihk35r99549xlbmvybwjx0p9mzyqi30dn3s6";
        name = "recipe";
      };
      packageRequires = with self; [ emacs ];
      meta = {
        homepage = "https://melpa.org/#/mu4e-conversation";
        license = nixpkgs.lib.licenses.free;
      };
    };

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

    lsp-haskell = super.melpaBuild {
      pname = "lsp-haskell";
      ename = "lsp-haskell";
      version = "20190602";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-haskell";
        rev = "8f2dbb6e827b1adce6360c56f795f29ecff1d7f6";
        sha256 = "00j6d5rpsi7h5jz54zpjmbpg38fda4xy67xc4x67r834493ldzlq";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/13d1a86dfe682f65daf529f9f62dd494fd860be9/recipes/lsp-haskell";
        sha256 = "0pdcxnfp8ng24bfk695wpx5wcdqnjrjsfpks0xicih3mcdm1x9l8";
        name = "recipe";
      };
      packageRequires = with self; [ haskell-mode lsp-mode ];
      meta = {
        homepage = "https://melpa.org/#/lsp-haskell";
        license = nixpkgs.lib.licenses.free;
        };
      };
    clang-format = super.melpaBuild {
      pname = "clang-format";
      ename = "clang-format";
      version = "20190211";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacsmirror";
        repo = "clang-format";
        rev = "0535810160aa6d7b48535c7b5b64891d1033e1c0";
        sha256 = "0036qabav8n2dbci0s608bfsb8nqvpfxifzswv8lp4xddh5avy4b";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/melpa/melpa/master/recipes/clang-format";
        sha256 = "0v8nvgjadzmsz088q6cgli5s99z45bz9qb508qln1yips42zn258";
        name = "recipe";
      };
      packageRequires = with self; [ cl-lib ];
      meta = {
        homepage = "https://melpa.org/#/clang-format";
        license = nixpkgs.lib.licenses.free;
      };
    };
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
    mu4e-conversation

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

    # cmake-mode

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

    dhall-mode
    purescript-mode
    psc-ide
    paredit
    yaml-mode
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
    xterm-color
    highlight-indent-guides
    emacs-libvterm
    docker
    dockerfile-mode
    qml-mode
    emojify
    disk-usage
    speed-type
  ];
  myEmacsPackagesNg =
    if nixpkgs.stdenv.isDarwin
    then nixpkgs.emacsPackagesNgGen nixpkgs.emacsMacport
    else nixpkgs.emacsPackagesNgGen (nixpkgs.emacs.override { inherit (nixpkgs) imagemagick; });
  emacs = (self.myEmacsPackagesNg.overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
