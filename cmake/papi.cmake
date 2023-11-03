
# Activate warnings, ecbuild macros check the compiler recognises the options
if(HAVE_PAPI)
  link_libraries ( -lpapi  )
  ecbuild_add_c_flags("-DHKPAPI")
endif()

