{ stdenv, fetchurl, fetchFromGitHub,
bc, kernel }:
stdenv.mkDerivation rec {
  # version = "1.0.0";
  version = "5.2.5.1";
  name = "rtl8821ce-${version}-${kernel.version}";
  src = fetchFromGitHub {
    owner = "tomaspinho";
    repo = "rtl8821ce";
    rev = "7cde2e70771dc15f07070b5ab9cb7fe74d6ae04d";
    sha256 = "17xd0dpbsjm7xqq9daxsnj38pkl5c6ysvlqplk5dhd5zblqq18zr";
    # date = 2018-09-20T16:14:52+01:00;
  };
  # src = fetchurl {
  #   url = "https://bugs.launchpad.net/ubuntu/+source/linux-oem/+bug/1740231/+attachment/5034985/+files/rtl8821CE_WiFi_linux_v5.2.5.1_26055.20180108_COEX20170310-1212.tar.gz";
  #   sha256 = "1xa2mdxqqld444k4ng5h17813p0gjawb2xpavfbyzaxc5vv3p7pa";
  # };
  hardeningDisable = [ "pic" "format" ];
  nativeBuildInputs = [bc] ++ kernel.moduleBuildDependencies;

  # patches = [ ./kernel-4_15-compat.patch ];

  # The driver source here includes a patch to build with the 4.15
  # kernel. If we are building with something older (e.g. 4.13), we
  # apply the reverse of that patch.
  # Here is the commit that added 4.15 compatibility:
  # https://github.com/endlessm/linux/commit/6978e0d629bdaf67729879bac4d3a853ea107f08

#        -e 's,CONFIG_POWER_SAVING = y,CONFIG_POWER_SAVING = n,' \

  patchPhase = ''
    sed -e 's,KSRC := /lib/modules/$(KVER)/build,KSRC := ${kernel.dev}/lib/modules/${kernel.modDirVersion}/build,' \
        -i Makefile
  ''#  + stdenv.lib.optionalString (stdenv.lib.versionAtLeast kernel.version "4.15") ''
  #   patch -p5 < ''${patches[0]}
  # ''


  #  + stdenv.lib.optionalString (stdenv.lib.versionOlder kernel.version "4.15") ''
  #   patch -p5 -R < ''${patches[0]}
  # ''
  ;
  installPhase = ''
    binDir="$out/lib/modules/${kernel.modDirVersion}/kernel/net/wireless/"
    mkdir -p "$binDir"
    cp 8821ce.ko "$binDir"
  '';

  meta = {
    description = "Kernel module driver for Realtek 8821ce wireless card";
    platforms = stdenv.lib.platforms.linux;
    license = stdenv.lib.licenses.gpl2;
  };
}
