{
  description = "Emacs Cutomization";
  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, emacs-overlay }:
    let my-emacs = import ../emacs.nix;
    in {
      overlay = final: prev: my-emacs final (emacs-overlay.overlay final prev);
    };
}
