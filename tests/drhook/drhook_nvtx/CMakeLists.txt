ecbuild_add_executable( TARGET drhook_nvtx_ex1
  SOURCES drhook_nvtx_ex1.F90 
  LIBS fiat
  LINKER_LANGUAGE Fortran
  NOINSTALL )

add_test (NAME fiat_test_drhook_nvtx_ex1 COMMAND fiat_test_drhook_nvtx_ex1.sh)

