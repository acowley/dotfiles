{ stdenv, fetchurl, fetchFromGitHub, fetchpatch, bc, kernel }:
stdenv.mkDerivation rec {
  # version = "1.0.0";
  version = "5.5.2_34066.20201216";
  name = "rtl8821ce-${version}-${kernel.version}";
  src = fetchFromGitHub {
    owner = "tomaspinho";
    repo = "rtl8821ce";
    rev = "14b536f0c9ad2d0abbdab8afc7ade684900ca9cf";
    sha256 = "0z7r7spsgn22gwv9pcmkdjn9ingi8jj7xkxasph8118h46fw8ip2";
    # date = 2020-12-16T01:00:05+00:00;
  };

  hardeningDisable = [ "pic" ];

  nativeBuildInputs = [ bc ];
  buildInputs = kernel.moduleBuildDependencies;

  prePatch = ''
    substituteInPlace ./Makefile \
      --replace /lib/modules/ "${kernel.dev}/lib/modules/" \
      --replace '$(shell uname -r)' "${kernel.modDirVersion}" \
      --replace /sbin/depmod \# \
      --replace '$(MODDESTDIR)' "$out/lib/modules/${kernel.modDirVersion}/kernel/net/wireless/"
  '';

  preInstall = ''
    mkdir -p "$out/lib/modules/${kernel.modDirVersion}/kernel/net/wireless/"
  '';

  meta = {
    description = "Kernel module driver for Realtek 8821ce wireless card";
    platforms = stdenv.lib.platforms.linux;
    license = stdenv.lib.licenses.gpl2;
  };
}
