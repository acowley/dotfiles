# self: super:
# let inherit (super) lib;
#     myOverlays = [(import ~/dotfiles/emacs.nix)]
#       ++ lib.optional (!(builtins.pathExists (builtins.toPath "/System/Library"))) [(import ~/dotfiles/rocm.nix)];
# in lib.foldl' (lib.flip lib.extends) (_: super) myOverlays self
# # in lib.fix toFix

let optOverlay = x: if builtins.pathExists x then [(import x)] else [];
in 
[# (import (import ./emacs-overlay/nix/sources.nix).emacs-overlay)
 #(import (import ./emacs-overlay/nix/sources.nix).collares)
 # (import ~/dotfiles/my-emacs/emacs.nix)
 # (import ~/dotfiles/nix/mu/overlay.nix)
 (import ~/dotfiles/nix/wordnet.nix)
 (import ~/dotfiles/nix/mylatex.nix)
 (import ~/dotfiles/nix/kwin-tiling.nix)
 # (import ~/dotfiles/nix/lean4.nix)

 # See https://github.com/nix-community/nix-direnv/issues/113#issuecomment-921328351
 # The fix https://github.com/NixOS/nixpkgs/pull/138334 was merged to staging-next
 # This hack rebuilds nix to avoid rebuilding everything else.
 # (self: super: {
 #   nixUnstable = super.nixUnstable.override {
 #     patches = [ ./nix/unset-is-macho.patch ];
 #   };
 # })
]
++ optOverlay ~/src/openconnect-sso/overlay.nix
# ++ (if builtins.pathExists ~/src/nixpkgs-mozilla/rust-overlay.nix
#     then [(import ~/src/nixpkgs-mozilla/rust-overlay.nix)
#           (import ~/src/nixpkgs-mozilla/rust-src-overlay.nix)
#           (import ~/src/nixpkgs-mozilla/firefox-overlay.nix)]
#     else [])
# ++ optOverlay ~/src/rust-analyzer-overlay/default.nix
# ++ optOverlay ~/dotfiles/nix/corectrl/overlay.nix
  # ++ optOverlay ~/src/nixos-rocm/default.nix

