self: super: {
  wordnet = super.wordnet.overrideAttrs (old: {
    version = "3.1db";
    newdict = super.fetchurl {
      url = "http://wordnetcode.princeton.edu/wn3.1.dict.tar.gz";
      sha256 = "0an226fl5zpav7vmgkdxnn044wzcailxc44vsdkp3k3fxzl8nz9z";
    };
    postInstall = old.postInstal or "" + ''
      (cd $out && tar xf $newdict)
    '';
  });
}
