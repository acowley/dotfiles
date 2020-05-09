self: pkgs: {
  mu = pkgs.mu.overrideAttrs (old: rec {
    version = "1.4.4";
    src = pkgs.fetchFromGitHub {
      owner  = "djcb";
      repo   = "mu";
      rev    = version;
      sha256 = "10snix81ng5vvjaq3ql8xyx4k130scymg87vrahj43s7n98bzgxs";
    };

    # This patch causes the mu4e-view-mode-hook to be called on
    # unread messages.  See https://github.com/djcb/mu/issues/1192
    # postPatch = old.postPatch or "" + ''
    #   sed "s/(unless mode-enabled (run-mode-hooks 'mu4e-view-mode-hook))/(run-mode-hooks 'mu4e-view-mode-hook)/" -i mu4e/mu4e-view.el  
    # '';
  });
}
