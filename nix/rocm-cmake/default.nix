{ stdenv, fetchFromGitHub, cmake }:
stdenv.mkDerivation {
  name = "rocm-cmake";
  version = "2018-03-30";
  src = fetchFromGitHub {
    owner = "RadeonOpenCompute";
    repo = "rocm-cmake";
    rev = "9e0a59ab3ca82d5fed6d5acaa5a5d891a4a65c86";
    sha256 = "1a4vxz6nb0dyfmssh2sxkaaq5iq5cdm9f9rsvzjwilwizih6lyd5";
  };
  nativeBuildInputs = [ cmake ];
}
