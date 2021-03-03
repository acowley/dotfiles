{lib, fetchFromGitHub, buildGoModule}:
buildGoModule rec {
  pname = "caire";
  version = "1.2.6";

  src = fetchFromGitHub {
    owner = "esimov";
    repo = "caire";
    rev = "v${version}";
    sha256 = "1gw5zki6hs57a5m2y5sgxc58ha3h4svhi8ln13mnw9qg0ag4cxak";
  };

  vendorSha256 = null;
  # subPackages = [ "." ]; 

  meta = with lib; {
    description = "Content aware image resize library";
    homepage = https://github.com/esimov/caire;
    license = licenses.mit;
    maintainers = with maintainers; [ ];
    platforms = platforms.linux ++ platforms.darwin;
  };
}
