self: super:
{
  hcc-clang-unwrapped = super.callPackage ./nix/hcc/clang.nix {};
  hcc-env = let hccself = {
    clang = super.ccWrapperFun {
      cc = self.hcc-clang-unwrapped;
      inherit (super.stdenv.cc) bintools libc nativeTools nativeLibc;
      extraPackages = [ super.libstdcxxHook ];
      extraBuildCommands = ''
        mkdir -p $out/include
        ln -s ${self.hcc-clang-unwrapped}/include $out/include/hcc
        ln -s $out/bin/clang++ $out/bin/hcc
      '';
    };
    stdenv = super.stdenv.override (drv: {
      allowedRequisites = null;
      cc = hccself.clang;
    });
  }; in hccself;
  hcc-clang = self.hcc-env.clang;
  hcc = self.callPackage ./nix/hcc/hcc.nix {};
  hip = self.callPackage ./nix/hip {};

  hipblas-check = self.callPackage ./nix/hipblas { doCheck = true; };
  hipblas = self.callPackage ./nix/hipblas {};
  rocblas-check = self.callPackage ./nix/rocblas { doCheck = true; };
  rocblas = self.callPackage ./nix/rocblas {};

  rocrand-check = self.callPackage ./nix/rocrand { doCheck = true; };
  rocrand = self.callPackage ./nix/rocrand {};
  rocfft-check = self.callPackage ./nix/rocfft { doCheck = true; };
  rocfft = self.callPackage ./nix/rocfft {};
  rocm-smi = self.callPackage ./nix/rocm-smi {};
  rocm-cmake = self.callPackage ./nix/rocm-cmake {};
  clang-ocl = self.callPackage ./nix/clang-ocl {};

  miopengemm = self.callPackage ./nix/miopengemm {};
  miopen = self.callPackage ./nix/miopen {};
  miopen-hip = self.callPackage ./nix/miopen { useHip = true; };

  rocprim = self.callPackage ./nix/rocprim {};
  rocm-thrust = self.callPackage ./nix/rocm-thrust {};
  google-gflags-dyn = super.google-gflags.overrideAttrs (old: {
    cmakeFlags = super.lib.filter (f: isNull (builtins.match ".*STATIC.*" f)) old.cmakeFlags;
  });

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
}
