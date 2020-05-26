self: pkgs: {
  mu = pkgs.mu.overrideAttrs (old: rec {
    version = "1.4.7";
    src = pkgs.fetchFromGitHub {
      owner  = "djcb";
      repo   = "mu";
      rev    = version;
      sha256 = "0inn720prhgxxc1napzd3xyzqgsvv70gqddsyzaa84h6946iz6v5";
    };
    # Do *not* mark messages as trashed. Just move them to the trash
    # folder, otherwise gmail holds on to things in All Mail. Also
    # mark them as seen and read so that if they are re-surfaced as
    # part of a conversation thread they do not appear as unread.
    # prePatch = old.prePatch or "" + ''
    #   sed 's/"+T-N"/"+S-u-N"/' -i mu4e/mu4e-mark.el
    # '';

    # This patch causes the mu4e-view-mode-hook to be called on
    # unread messages.  See https://github.com/djcb/mu/issues/1192
    # postPatch = old.postPatch or "" + ''
    #   sed "s/(unless mode-enabled (run-mode-hooks 'mu4e-view-mode-hook))/(run-mode-hooks 'mu4e-view-mode-hook)/" -i mu4e/mu4e-view.el  
    # '';
  });
}
