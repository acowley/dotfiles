self: nixpkgs: {
  myEmacsPackageOverrides = self: super: super.melpaPackages // {
    inherit (super) pdf-tools;
    inherit (super) emacs-libvterm;
    org = self.orgPackages.org;
    org-plus-contrib = self.orgPackages.org-plus-contrib;
    orgPackages = {
      org = super.orgPackages.org.overrideAttrs (old : {
        patches = (old.patches or []) ++ [ ./org-short-caption.patch ];
        fixupPhase = ''
          cd $out/share/emacs/site-lisp/elpa/org* && patch -p1 < $patches
          cd $out/share/emacs/site-lisp/elpa/org* && emacs --batch -Q --eval "(byte-recompile-directory \"$PWD\" 0)"
        '';
      });
      org-plus-contrib = super.orgPackages.org-plus-contrib.overrideAttrs (old: {
        patches = (old.patches or []) ++ [ ./org-short-caption.patch ];
        fixupPhase = ''
          cd $out/share/emacs/site-lisp/elpa/org* && patch -p1 < $patches
          cd $out/share/emacs/site-lisp/elpa/org* && emacs --batch -Q --eval "(byte-recompile-directory \"$PWD\" 0)"
       '';
      });
    };
    org-roam = super.melpaBuild {
      pname = "org-roam";
      version = "20200219";
      src = nixpkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "org-roam";
        rev = "0346d3b16c42e6a05e34daa2fb91dddf8cfe714b";
        sha256 = "0v4wlw5q7nbc1659miyzxgdgrbirbbh9p3qk4d1gazbqc1k52wbz";
      };
      packageRequires = [ self.f self.dash ];
      recipe = nixpkgs.writeText "recipe" ''
        (org-roam :repo "jethrokuan/org-roam" :fetcher github)
      '';
    };
    disk-usage = super.elpaBuild {
      pname = "disk-usage";
      ename = "disk-usage";
      version = "20190422";
      src = (nixpkgs.fetchFromGitLab {
        owner = "ambrevar";
        repo = "emacs-disk-usage";
        rev = "b0fb8af34291a49b041eab8b5570e7bc8433a8d8";
        sha256 = "0hv2gsd8k5fbjgckgiyisq4rn1i7y4rchbjy8kmixjv6mx563bll";
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
      version = "20191002";
      src = nixpkgs.fetchFromGitHub {
        owner = "MaskRay";
        repo = "emacs-ccls";
        rev = "b1acc336f27d8a3bbc750c2dc3be915a4ac1afea";
        sha256 = "1qgfxc5d1hb32ks1fxpx7agpw7dvnkz99wydlflc9fqq75g8v142";
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
      version = "20200121";
      # src = nixpkgs.fetchFromGitHub {
      #   owner = "ashiklom";
      #   repo = "helm-swoop";
      #   rev = "6a881d81fbd1ff550ff22a118be18141d808f91c";
      #   sha256 = "07vgzykv4p7kbbxj06pjqychh62grm0c5d42073c6z4097dpdim4";
      # };
      src = nixpkgs.fetchFromGitHub {
        owner = "emacsorphanage";
        repo = "helm-swoop";
        rev = "9324d8c196ab2a86fde7142f159e081b87a4d277";
        sha256 = "1nznfrnzfbxa5qlwbddjma96k93f9hr7jv9sqx3krc0i1061nbg8";
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
      version = "20200224";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-mode";
        rev = "9982b7ed5d96eb46cc1de0501de3b66944b90ff4";
        sha256 = "1l3ibbvwzz8dy7klxpkzrdxc5l8lxg2w0qazasq4nlviv364v4b2";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/1a7b69312e688211089a23b75910c05efb507e35/recipes/lsp-mode";
        sha256 = "0cklwllqxzsvs4wvvvsc1pqpmp9w99m8wimpby6v6wlijfg6y1m9";
        name = "recipe";
      };
      packageRequires = with self; [ emacs ];
      # patches = [ ./lsp-mode-hash-check.patch ];
      meta = {
        homepage = "https://melpa.org/#/lsp-mode";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-ui = super.melpaBuild {
      pname = "lsp-ui";
      ename = "lsp-ui";
      version = "20200224";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-ui";
        rev = "da9788b42723b71318ccd0810d73d2147bc712f1";
        sha256 = "1inihjl39ahvgi4h1q7mz8mbwvqgs73i3zxklhrhfrdxmmbizq9g";
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
      patches = [ ./lsp-ui-window-change-no-hide.patch ];
      meta = {
        homepage = "https://melpa.org/#/lsp-ui";
        license = nixpkgs.lib.licenses.free;
      };
    };
    company-lsp = super.melpaBuild {
      pname = "company-lsp";
      ename = "company-lsp";
      version = "20190612";
      src = nixpkgs.fetchFromGitHub {
        owner = "tigersoldier";
        repo = "company-lsp";
        rev = "f921ffa0cdc542c21dc3dd85f2c93df4288e83bd";
        sha256 = "0dd2plznnnc2l1gqhsxnvrs8n1scp6zbcd4457wrq9z2f7pb5ig2";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/13d1a86dfe682f65daf529f9f62dd494fd860be9/recipes/company-lsp";
        sha256 = "09nbi6vxw8l26gfgsc1k3bx4m8i1px1b0jxaywszky5bv4fdy03l";
        name = "recipe";
      };
      packageRequires = with self; [ company dash emacs lsp-mode s ];
      patches = [ ./company-lsp-hash-check.patch ];
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

    doom-modeline =
      super.melpaPackages.doom-modeline.overrideAttrs (_: {
        # patches = [./god-segment.patch];
        patches = [./god-segment-project.patch];
    });

    lsp-haskell = super.melpaBuild {
      pname = "lsp-haskell";
      ename = "lsp-haskell";
      version = "20191230";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-haskell";
        rev = "6d481f97e62b0fd2455e8f7a36429981277445b1";
        sha256 = "0ljflzdjzsafgqqq9fdajrcm0rk4xaki2h5gbsbkgn8z65a2xh6h";
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
    # clang-format = super.melpaBuild {
    #   pname = "clang-format";
    #   ename = "clang-format";
    #   version = "20190211";
    #   src = nixpkgs.fetchFromGitHub {
    #     owner = "emacsmirror";
    #     repo = "clang-format";
    #     rev = "0535810160aa6d7b48535c7b5b64891d1033e1c0";
    #     sha256 = "0036qabav8n2dbci0s608bfsb8nqvpfxifzswv8lp4xddh5avy4b";
    #   };
    #   recipe = nixpkgs.fetchurl {
    #     url = "https://raw.githubusercontent.com/melpa/melpa/master/recipes/clang-format";
    #     sha256 = "0v8nvgjadzmsz088q6cgli5s99z45bz9qb508qln1yips42zn258";
    #     name = "recipe";
    #   };
    #   packageRequires = with self; [ cl-lib ];
    #   meta = {
    #     homepage = "https://melpa.org/#/clang-format";
    #     license = nixpkgs.lib.licenses.free;
    #   };
    # };
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
    all-the-icons
    impatient-mode
    esup
    direnv
    dired-du
    mu4e-conversation
    ace-window

    # org packages
    orgPackages.org-plus-contrib
    org-bullets
    org-sticky-header
    org-table-sticky-header
    org-journal
    ox-reveal
    ox-tufte
    ox-gfm
    ob-ipython
    org-noter
    outorg
    outshine
    ox-clip
    org-mime
    org-ref
    org-roam
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
    ripgrep
    xterm-color
    highlight-indent-guides
    emacs-libvterm
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
    else nixpkgs.emacsPackagesFor ((nixpkgs.emacs.override { inherit (nixpkgs) imagemagick; srcRepo = true; }).overrideAttrs (old: rec {
      name = "emacs-${version}${versionModifier}";
      version = "28.0";
      versionModifier = ".50";
      src = nixpkgs.fetchgit {
        url = "git://git.sv.gnu.org/emacs.git";

        rev = "46fefb09745abbcdb4b56d80cd2bbd545afc39e1";
        sha256 = "1mmha7lp5bn7snf6d5mjs9979cbsav0h59rxsn5myyplg194pn6f";

        # rev = "41450a8ea5a156a34f6641a0768cadb174fa261c";
        # sha256 = "1mprbwbbacnmh620416ysfmpdzgglvix6znf8yrj1vyx9z8y7784";
      };
      patches = [];
      prePatch = ''
        sed 's/\([[:space:]]*\)\(LC_ALL=C $(RUN_TEMACS) -batch $(BUILD_DETAILS) -l loadup --temacs=dump\)/\1env -i \2/' -i src/Makefile.in
        sed 's/\((tramp-compat-process-running-p "gvfs-fuse-daemon")\)/(tramp-compat-process-running-p ".gvfsd-fuse-wrapped") \1/' -i lisp/net/tramp-gvfs.el
        sed 's/error ("Attempt to shape unibyte text");/;/g' -i src/composite.c
      '';
      buildInputs = (old.buildInputs or []) ++ [nixpkgs.jansson];
    }));
  emacs = (self.myEmacsPackagesFor.overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
