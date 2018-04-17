{ stdenv, fetchFromGitHub, writeText }:
stdenv.mkDerivation {
  name = "rocm-thrust";
  version = "2018-04-02";
  src = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "Thrust";
    rev = "991a1aa6c027c3465825ad1c879dc4427a873568";
    sha256 = "0xfz1gb4zfhv5pd6ysphrxbnvqcq292vh4giwncj4rz0b3y283y6";
  };
  cub = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "cub-hip";
    rev = "hip_port_1.7.4_caffe2";
    sha256 = "1dfgbz0b43n0hdgxjwvxh2dl5wi3w291nm32w475ihw8v8z40ijn";
  };
  buildInputs = [];
  builder = writeText "builder.sh" ''
    source $stdenv/setup
    mkdir -p $out/include/thrust/system/cuda/detail/cub-hip
    cp -rs $src/thrust $out/include
    ln -s $cub/cub $out/include/thrust/system/cuda/detail/cub-hip/cub
    ln -sf $out/include/thrust/system/cuda/detail/cub-hip/cub $out/include/thrust/system/cuda/detail/cub
  '';
}
