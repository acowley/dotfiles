{ stdenv, fetchgit, cmake, boost, eigen }:
# let version = "3.2.1"; in
let version = "2018-02-20"; in
stdenv.mkDerivation {
  name = "gtsam-${version}";
  src = fetchgit {
    url = "https://bitbucket.org/gtborg/gtsam.git";
    rev = "cc25ece0551ba6ad1116046f5d4a19436d952d00";
    sha256 = "0y7fb36k5j6rq5v35qd46qp2zyv23r9wbdnqfpng36ikspnnpg1j";
  };
  cmakeFlags = [
    "-DGTSAM_USE_SYSTEM_EIGEN=ON"
    "-DGTSAM_TYPEDEF_POINTS_TO_VECTORS=ON"
    "-DGTSAM_BUILD_WRAP=OFF"
  ];
#    "-DGTSAM_BUILD_TESTS=OFF"
  nativeBuildInputs = [ cmake boost eigen ];
  meta = with stdenv.lib; {
    description = "A library of C++ classes that implement smoothing and mapping (SAM) in robotics and vision";
    homepage = https://research.cc.gatech.edu/borg/gtsam/;
    platforms = platforms.unix;
    license = licenses.bsd3;
    maintainers = with maintainers; [ acowley ];
  };
}
