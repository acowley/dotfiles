{ stdenv, fetchurl,# fetchFromGitHub,
bc, kernel }:
stdenv.mkDerivation rec {
  # version = "1.0.0";
  version = "5.2.5.1";
  name = "rtl8821ce-${version}-${kernel.version}";
  # src = fetchFromGitHub {
  #   owner = "tomaspinho";
  #   repo = "rtl8821ce";
  #   rev = "6ba8930b5894d653592df298ef39e6a35ac5d33a";
  #   sha256 = "1pwjx6g0cqcrmx1m52rnl7l4982mfnfhpqyd63pbm61ykam0lc0n";
  # };
  src = fetchurl {
    url = "https://bugs.launchpad.net/ubuntu/+source/linux-oem/+bug/1740231/+attachment/5034985/+files/rtl8821CE_WiFi_linux_v5.2.5.1_26055.20180108_COEX20170310-1212.tar.gz";
    sha256 = "1xa2mdxqqld444k4ng5h17813p0gjawb2xpavfbyzaxc5vv3p7pa";
  };
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
  ''#  + stdenv.lib.optionalString (stdenv.lib.versionOlder kernel.version "4.15") ''
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
