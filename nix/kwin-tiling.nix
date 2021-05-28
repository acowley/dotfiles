self: super: {
  kwin-tiling = super.kwin-tiling.overrideAttrs (_: rec {
    version = "2.4-unstable-2020-12-17";
    src = super.fetchFromGitHub {
      owner = "kwin-scripts";
      repo = "kwin-tiling";
      rev = "a240b4327892305008525fed96f9f9227c904893";
      sha256 = "sha256:0d0hr0c7zl05j0k55drck9ghm66mjdfvl858zr8vx6ff8fva1i4f";
    };

    # NOTE: We need to copy the `installPhase` here because it
    # references `src` and we need that reference to resolve to our
    # updated `src`.

    # 1. --global still installs to $HOME/.local/share so we use --packageroot
    # 2. plasmapkg2 doesn't copy metadata.desktop into place, so we do that manually
    installPhase = ''
      runHook preInstall

      plasmapkg2 --type kwinscript --install ${src} --packageroot $out/share/kwin/scripts
      install -Dm644 ${src}/metadata.desktop $out/share/kservices5/kwin-script-tiling.desktop

      runHook postInstall
    '';
  });
}
