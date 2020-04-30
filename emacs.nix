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
    org-roam = super.melpaBuild rec {
      pname = "org-roam";
      version = "1.1.0";
      src = nixpkgs.fetchFromGitHub {
        owner = "jethrokuan";
        repo = "org-roam";
        rev = "v${version}";
        sha256 = "18ljww204kf1pbgrrnx7bn6177lw1bs3npywbx2k1b5j35c3j8xv";
      };
      packageRequires = [ self.f self.dash self.async 
                          self.emacsql self.emacsql-sqlite ];
      recipe = nixpkgs.writeText "recipe" ''
        (org-roam :repo "jethrokuan/org-roam" :fetcher github)
      '';
    };
    helm-bibtex = super.helm-bibtex.overrideAttrs (old: {
      src = nixpkgs.fetchFromGitHub {
        owner = "tmalsburg";
        repo = "helm-bibtex";
        rev = "4b7bb7944085f11a23a0999178e1328c71fe998e";
        sha256 = "1hawvxg25m060fqnz8bvw3j6p1k00f7mcd0bhkplr78krgls7lcl";
      };
    });
    org-roam-bibtex = super.melpaBuild rec {
      pname = "org-roam-bibtex";
      version = "0.1.0";
      src = nixpkgs.fetchFromGitHub {
        owner = "zaeph";
        repo = "org-roam-bibtex";
        rev = "v${version}";
        sha256 = "14f3d1yiidglwbygb5swk44fvky7cla3r11i4zx56hrf8lxjzhp6";
      };
      packageRequires = [ self.f self.s self.org self.org-roam
                          self.bibtex-completion ];
      recipe = nixpkgs.writeText "recipe" ''
        (org-roam-bibtex :repo "zaeph/org-roam-bibtex" :fetcher github)
      '';
    };
    bibtex-completion = super.melpaBuild {
      pname = "bibtex-completion";
      version = "20200427";
      src = nixpkgs.fetchFromGitHub {
        owner = "tmalsburg";
        repo = "helm-bibtex";
        rev = "4b7bb7944085f11a23a0999178e1328c71fe998e";
        sha256 = "1hawvxg25m060fqnz8bvw3j6p1k00f7mcd0bhkplr78krgls7lcl";
      };
      packageRequires = [ self.parsebib self.s self.dash self.f self.cl-lib
                          self.biblio ];
      recipe = nixpkgs.writeText "recipe" ''
        (bibtex-completion :repo "tmalsburg/helm-bibtex" :fetcher github :files ("bibtex-completion.el"))
      '';
    };
    company-box = super.melpaBuild {
      pname = "company-box";
      ename = "company-box";
      version = "20200429";
      src = nixpkgs.fetchFromGitHub {
        owner = "sebastiencs";
        repo = "company-box";
        rev = "3814fcb14e92f4b85b19e664e216a7c8d5c7144d";
        sha256 = "0ncc0gmgpnn1q7h7lkxbnfikryq8mqgn6vr8giz8vsngzbv063k6";
      };
      packageRequires = [ super.emacs self.company self.dash
                          self.dash-functional super.all-the-icons ];
      recipe = nixpkgs.writeText "recipe" ''
        (company-box :repo "sebastiencs/company-box" :fetcher github :files (:defaults "images"))
      '';
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
      patches = [
        (nixpkgs.fetchpatch {
          # Replace (require 'lsp) with (require 'lsp-mode)
          url = "https://patch-diff.githubusercontent.com/raw/MaskRay/emacs-ccls/pull/81.patch";
          sha256 = "08n987bwwi0qkaggc976vdkanj9rgq46vhddswa7rw2vny30vlks";
        })
      ];

      packageRequires = with self; [dash emacs self.lsp-mode ht spinner projectile];
      meta = {
        homepage = "https://melpa.org/#/ccls";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-mode = super.melpaBuild {
      pname = "lsp-mode";
      ename = "lsp-mode";
      version = "20200406";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-mode";
        rev = "c2de69cc00c69b2d49e204e8bfa6af1f11a15877";
        sha256 = "01k7frka2x4h0nk86ym2jqvcac527zxqzf0gi66c0rrhsfw1al8w";
      };
      recipe = nixpkgs.fetchurl {
        url = "https://raw.githubusercontent.com/milkypostman/melpa/1a7b69312e688211089a23b75910c05efb507e35/recipes/lsp-mode";
        sha256 = "0cklwllqxzsvs4wvvvsc1pqpmp9w99m8wimpby6v6wlijfg6y1m9";
        name = "recipe";
      };
      packageRequires = with self; [ emacs f ht dash-functional spinner lv markdown-mode ];
      meta = {
        homepage = "https://melpa.org/#/lsp-mode";
        license = nixpkgs.lib.licenses.free;
      };
    };
    lsp-ui = super.melpaBuild {
      pname = "lsp-ui";
      ename = "lsp-ui";
      version = "20200312";
      src = nixpkgs.fetchFromGitHub {
        owner = "emacs-lsp";
        repo = "lsp-ui";
        rev = "134d9b725d21f8889f3dc72dddc418c6c6561f0e";
        sha256 = "1ajza32nj4l5m0x9kghlwc2plavd507wajna6cdk5z276lyrn38a";
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
    # esup
    benchmark-init
    direnv
    dired-du
    mu4e-conversation
    ace-window

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
    else nixpkgs.emacsPackagesFor ((nixpkgs.emacs.override { imagemagick = nixpkgs.imagemagickBig; srcRepo = true; }).overrideAttrs (old: rec {
      name = "emacs-${version}${versionModifier}";
      version = "28.0";
      versionModifier = ".50";
      src = nixpkgs.fetchgit {
        url = "git://git.sv.gnu.org/emacs.git";
        # rev = "ef4440a9c80b284d46ecce47f4d387f132bbf374";
        # sha256 = "1k3yhm9mg44ql1pinrl93dmcfj6q84j2z3gj0a9jivsmpybs5wy9";
        rev = "421eeff243af683bf0b7c6d9181650a1c6900f9b";
        sha256 = "09ci3cyzl3vbvag8ldyrqvg0av0057b8qnq4j7whgp91w52m3vlp";
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
  emacs = (self.myEmacsPackagesFor.overrideScope' self.myEmacsPackageOverrides).emacsWithPackages self.myEmacsPackages;
}
