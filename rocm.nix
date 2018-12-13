self: super:
{
  rocprim = self.callPackage ./nix/rocprim {};
  rocm-thrust = self.callPackage ./nix/rocm-thrust {};
  google-gflags-dyn = super.google-gflags.overrideAttrs (old: {
    cmakeFlags = super.lib.filter (f: isNull (builtins.match ".*STATIC.*" f)) old.cmakeFlags;
  });

  # rocprofiler = self.callPackage ./nix/rocprofiler {};

  rocm-caffe2 = self.callPackage ./nix/rocm-caffe2 {
    eigen3 = super.eigen3_3;
    inherit (super.python3Packages) python future six numpy pydot;
    protobuf = super.protobuf3_1;
    python-protobuf = super.python3Packages.protobuf3_1;
    # Used only for image loading.
    opencv3 = super.opencv3WithoutCuda;
    google-gflags = self.google-gflags-dyn;
  };
  rocm-caffe2-check = self.rocm-caffe2.override { doCheck = true; };
  rocm-caffe2-python = super.callPackage ./nix/rocm-caffe2/rocm-caffe2-python.nix {
    inherit (super.python3Packages) buildPythonPackage six numpy;
    python-protobuf = super.python3Packages.protobuf3_1;
  };

  hipcaffe = super.callPackage ./nix/hipcaffe {};
  # rcp = super.callPackage ./nix/rcp {};

  mxnet = super.callPackage ./nix/mxnet { lapack = self.lapack_3_8; };
}
