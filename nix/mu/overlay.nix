self: pkgs: {
  mu = pkgs.mu.overrideAttrs (old: rec {
    version = "1.4.1";
    src = pkgs.fetchFromGitHub {
      owner  = "djcb";
      repo   = "mu";
      rev    = version;
      sha256 = "0q2ik7fj5k9i76js4ijyxbgrwqff437lass0sd5if2r40rqh0as0";
    };

    # This patch causes the mu4e-view-mode-hook to be called on
    # unread messages.  See https://github.com/djcb/mu/issues/1192
    # postPatch = old.postPatch or "" + ''
    #   sed "s/(unless mode-enabled (run-mode-hooks 'mu4e-view-mode-hook))/(run-mode-hooks 'mu4e-view-mode-hook)/" -i mu4e/mu4e-view.el  
    # '';
  });
}
