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
      version = "20190225";
      src = nixpkgs.fetchFromGitHub {
        owner = "MaskRay";
        repo = "emacs-ccls";
        rev = "93c18aee1e0b07392c4dc3d0462d6a9d918b9728";
        sha256 = "03znjx8irq97s8677l0bz63ma18r7fack59bmanr5rhan59s0bq9";
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
    # Fix an incompatibility with newer helm that breaks helm-swoop--edit
    helm-swoop = super.melpaBuild {
      pname = "helm-swoop";
      ename = "helm-swoop";
      version = "20190103.000";
      src = nixpkgs.fetchFromGitHub {
        owner = "ashiklom";
        repo = "helm-swoop";
        rev = "6a881d81fbd1ff550ff22a118be18141d808f91c";
        sha256 = "07vgzykv4p7kbbxj06pjqychh62grm0c5d42073c6z4097dpdim4";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/855ea20024b606314f8590129259747cac0bcc97/recipes/helm-swoop";
        sha256 = "1b3nyh4h5kcvwam539va4gzxa3rl4a0rdcriif21yq340yifjbdx";
        name = "recipe";
      };
      packageRequires = with super; [ emacs helm ];
      meta = {
        homepage = "https://melpa.org/#/helm-swoop";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-mode = super.melpaBuild {
      pname = "lsp-mode";
      ename = "lsp-mode";
      version = "20190327";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-mode";
        rev = "a91cedfa2923e5261c59b02f620c6206d39e9c3e";
        sha256 = "0hljz792501rsm7d44wphh92d6rk949a07rvrvzgnmyq5kfxyfgd";
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
      version = "20190318";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-ui";
        rev = "8d855dba9bb06405b62c474f6d753d85a33ae469";
        sha256 = "0bmkj0kn8z0lhizxqpz677v4jzjak5fykh9v53znvlpp2kixd3vy";
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
      version = "20190326";
      src = nixpkgs.fetchFromGitHub {
        owner = "tigersoldier";
        repo = "company-lsp";
        rev = "4218f180688eb3ecba454d00b0e68931a0aef5c9";
        sha256 = "0pq4zxviy1cxp28dfnnrxxsi57g0d91chg1pshdhgcpcd7rb53rh";
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
      version = "20181223.2357";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-haskell";
        rev = "533970d5552c4820aa45901ba89565f3419d991c";
        sha256 = "0xah24q8c62kk0m5ivhx51a3h46vlc626qsh8rlysdsdv59121sa";
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
  ] ++ nixpkgs.lib.optionals (builtins.pathExists ~/src/intero) [
    # This is a hacky way of not building these from source on
    # machines where we do not expect to use them.
    structured-haskell-mode
    intero
  ] ++ [
    lsp-mode
    lsp-ui
    lsp-haskell
    ccls

    cuda-mode
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
  ];
  myEmacsPackagesNg =
    if nixpkgs.stdenv.isDarwin
    then nixpkgs.emacsPackagesNgGen nixpkgs.emacsMacport
    else nixpkgs.emacsPackagesNgGen (nixpkgs.emacs.override { inherit (nixpkgs) imagemagick; });
  emacs = (self.myEmacsPackagesNg.overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
