set -x
if [[ $HOSTNAME =~ leonardo ]] ; then
  echo "on Leonardo"
  #ml purge
  # Load modules
  ml intel-oneapi-compilers/2023.2.1
  ml intel-oneapi-mkl/2023.2.0
  ml intel-oneapi-tbb/2021.10.0
  ml use /leonardo/pub/userexternal/lanton00/spack-0.21.0-05/modules
  #ml spack
  ml openmpi/4.1.6--intel--2021.10.0
  ml
  export ecbuild_ROOT=../ecbuild
  export fckit_ROOT=../fckit

else
  # lumi
  ml LUMI/24.03
  ml partition/C
  ml
  export ecbuild_ROOT=../ecbuild
  #export MPI_HOME=<path-to-MPI>
  export fckit_ROOT=../fckit
  #export CC=
  #export FC=<path-to-Fortran-compiler>
  #export CXX=<path-to-C++-compiler> 
fi
set +x
