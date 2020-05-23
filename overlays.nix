# self: super:
# let inherit (super) lib;
#     myOverlays = [(import ~/dotfiles/emacs.nix)]
#       ++ lib.optional (!(builtins.pathExists (builtins.toPath "/System/Library"))) [(import ~/dotfiles/rocm.nix)];
# in lib.foldl' (lib.flip lib.extends) (_: super) myOverlays self
# # in lib.fix toFix

let optOverlay = x: if builtins.pathExists x then [(import x)] else [];
in 
[(import (import ./emacs-overlay/nix/sources.nix).emacs-overlay)
 (import ~/dotfiles/emacs.nix)
 (import ~/dotfiles/nix/mu/overlay.nix)
 (import ~/dotfiles/nix/wordnet.nix)]
++ (if builtins.pathExists ~/src/nixpkgs-mozilla/rust-overlay.nix
    then [(import ~/src/nixpkgs-mozilla/rust-overlay.nix)
          (import ~/src/nixpkgs-mozilla/rust-src-overlay.nix)]
    else [])
++ optOverlay ~/src/rust-analyzer-overlay/default.nix
++ optOverlay ~/dotfiles/nix/corectrl/overlay.nix
++ (if builtins.pathExists (builtins.toPath "/System/Library")
    then []
    else if builtins.pathExists ~/src/nixos-rocm
	 then [ (import ~/src/nixos-rocm/default.nix) ]
	 else [])

