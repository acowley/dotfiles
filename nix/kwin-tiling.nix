self: super: {
  kwin-tiling = super.kwin-tiling.overrideAttrs (_: rec {
    version = "2.4-unstable-2020-12-17";
    src = super.fetchFromGitHub {
      owner = "kwin-scripts";
      repo = "kwin-tiling";
      rev = "660e0cbc58452b0cb8cbf6ea6f2487332ada2655";
      hash = "sha256-xixZ36b+XCfDeqKS+P9bESnKfjPVuEaKcsPKRU62760=";
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
