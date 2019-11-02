# self: super:
# let inherit (super) lib;
#     myOverlays = [(import ~/dotfiles/emacs.nix)]
#       ++ lib.optional (!(builtins.pathExists (builtins.toPath "/System/Library"))) [(import ~/dotfiles/rocm.nix)];
# in lib.foldl' (lib.flip lib.extends) (_: super) myOverlays self
# # in lib.fix toFix
[(import ~/dotfiles/emacs.nix)]
++ (if builtins.pathExists ~/src/nixpkgs-mozilla/rust-overlay.nix
    then [(import ~/src/nixpkgs-mozilla/rust-overlay.nix)
          (import ~/src/nixpkgs-mozilla/rust-src-overlay.nix)]
    else [])
++ (if builtins.pathExists (builtins.toPath "/System/Library")
    then []
    else [ (import ~/src/nixos-rocm/pkgs/all-packages.nix ["gfx803" "gfx900" "gfx906"]) ])
