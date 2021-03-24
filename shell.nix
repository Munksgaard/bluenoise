{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.pkgconfig
    pkgs.opencl-headers
    pkgs.intel-compute-runtime
    pkgs.ocl-icd
    pkgs.imagemagick
    pkgs.pandoc
  ];
}
