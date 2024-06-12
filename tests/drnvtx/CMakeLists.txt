ecbuild_add_executable( TARGET drnvtx_ex1
  SOURCES drnvtx_ex1.F90 
  LIBS fiat
  LINKER_LANGUAGE Fortran
  NOINSTALL )

add_test (NAME fiat_test_drnvtx_ex1 COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/fiat_test_drnvtx_ex1.sh)

