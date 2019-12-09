{ stdenv, fetchurl, fetchFromGitHub, fetchpatch, bc, kernel }:
stdenv.mkDerivation rec {
  # version = "1.0.0";
  version = "5.5.2_34066.20190917";
  name = "rtl8821ce-${version}-${kernel.version}";
  src = fetchFromGitHub {
    owner = "tomaspinho";
    repo = "rtl8821ce";
    # rev = "55b90f46e203c6f46f731b039497b9aac8ea67ac";
    # sha256 = "16rl6gzhdkzb7b96ckyf9mvgcvj34a7h15v7b2ab4jw17c9kgjvn";
    rev = "7c4f8274d3cd5e3e88a02fdaacee901cce43f81a";
    sha256 = "0i10n14x1q4rqhaqvj2hc2483jayz179kyhh0fqny9c8r7r0538k";
  };

  hardeningDisable = [ "pic" ];

  nativeBuildInputs = [ bc ];
  buildInputs = kernel.moduleBuildDependencies;

  # Revert a patch adding in-tree build detection logic that fails with nix
  # patches = [ (fetchpatch {
  #   url = "https://github.com/tomaspinho/rtl8821ce/commit/bde9b3208a509588d64816895b882b91358ac8bc.patch";
  #   sha256 = "1c06b4gcsyvlxiz8aakyd5621vr8mw4a2r9kzrhjs6p3jn5bg8dl";
  #   revert = true;
  # })];

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
