{ stdenv, fetchFromGitHub, writeText }:
stdenv.mkDerivation {
  name = "rocm-thrust";
  version = "2018-04-25";
  src = fetchFromGitHub {
    owner = "ROCmSoftwarePlatform";
    repo = "Thrust";
    rev = "d42fef5505455bba451dea07c046c7b7ee050c2e";
    sha256 = "1knprnwnvj3z96ilb89hgrar79nwc6iair0sbv1f63yxb27fz08n";
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
