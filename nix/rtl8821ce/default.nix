{ stdenv, fetchurl, fetchFromGitHub, fetchpatch, bc, kernel }:
stdenv.mkDerivation rec {
  # version = "1.0.0";
  version = "5.5.2_34066.20190917";
  name = "rtl8821ce-${version}-${kernel.version}";
  src = fetchFromGitHub {
    owner = "tomaspinho";
    repo = "rtl8821ce";
    rev = "2e7c099370756e6225166fdb3f203ae04b53fc7e";
    sha256 = "1mf0j0nk1ddp2wfy9143h7qq4cvf2crk5g756icswvcqxc4sjw90";
    # date = 2020-02-15T14:43:41+00:00;
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
