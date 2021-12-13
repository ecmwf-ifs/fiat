
# activate warnings, ecbuild macros check the compiler recognises the options
if(HAVE_WARNINGS)
  ecbuild_add_c_flags("-pedantic"                              NO_FAIL)
  ecbuild_add_c_flags("-Wall"                                  NO_FAIL)
  ecbuild_add_c_flags("-Wextra"                                NO_FAIL)
  ecbuild_add_c_flags("-Wno-unused-parameter"                  NO_FAIL)
  ecbuild_add_c_flags("-Wno-unused-variable"                   NO_FAIL)
  ecbuild_add_c_flags("-Wno-gnu-zero-variadic-macro-arguments" NO_FAIL)
endif()
ecbuild_add_c_flags("-Wno-deprecated-declarations" NO_FAIL)
