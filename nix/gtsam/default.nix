{ stdenv, fetchgit, cmake, boost, eigen }:
# let version = "3.2.1"; in
let version = "2017-09-21"; in
stdenv.mkDerivation {
  name = "gtsam-${version}";
  src = fetchgit {
    url = "https://bitbucket.org/gtborg/gtsam.git";
    rev = "c82fe1fde2fc988b6bde5e4798b66129bbb5da19";
    sha256 = "1r8lisdiqpci5x7nbvf56637kigxkp5jfhm4vn7vqgizbif1pl25";
  };
  cmakeFlags = [
    "-DGTSAM_USE_SYSTEM_EIGEN=ON"
    "-DGTSAM_TYPEDEF_POINTS_TO_VECTORS=ON"
    "-DGTSAM_BUILD_WRAP=OFF"
  ];

  nativeBuildInputs = [ cmake boost eigen ];
  meta = with stdenv.lib; {
    description = "A library of C++ classes that implement smoothing and mapping (SAM) in robotics and vision";
    homepage = https://research.cc.gatech.edu/borg/gtsam/;
    platforms = platforms.unix;
    license = licenses.bsd3;
    maintainers = with maintainers; [ acowley ];
  };
}
