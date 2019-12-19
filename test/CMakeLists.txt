# Make sure the test executables end up on build/test/, not build/bin/
unset(CMAKE_RUNTIME_OUTPUT_DIRECTORY)

# Let's catch implicit uses of routines in the test prorams and libraries
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Werror=implicit-interface -Werror=implicit-procedure")

# A library of test helper routines etc.
add_library(grasptest STATIC
    lib/grasptest.f90
)
setup_fortran_modules(grasptest)

# Tests for libgrasp
# ------------------
# libgrasp constants
add_executable(test.libgrasp_constants libgrasp/constants.f90)
target_link_libraries_Fortran(test.libgrasp_constants PUBLIC grasp grasptest)
add_test(libgrasp_constants test.libgrasp_constants)
# Reading isodata files
add_executable(test.libgrasp_isodata_read libgrasp/isodata/read.f90)
target_link_libraries_Fortran(test.libgrasp_isodata_read PUBLIC grasp grasptest)
add_test(libgrasp_isodata_read test.libgrasp_isodata_read)
set_property(TEST libgrasp_isodata_read PROPERTY ENVIRONMENT
    "GRASP_ISODATA_SOURCE=${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/isodata"
)
# Writing isodata files
add_executable(test.libgrasp_isodata_write libgrasp/isodata/write.f90)
target_link_libraries_Fortran(test.libgrasp_isodata_write PUBLIC grasp grasptest)
add_test(libgrasp_isodata_write test.libgrasp_isodata_write)
# Make sure the written isodata files are byte-by-byte identical
add_test(NAME libgrasp_isodata_write.zeros
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/lib/filematch.sh
    "${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/isodata/isodata_write.zeros.ref"
    "isodata_write.zeros.test"
)
add_test(NAME libgrasp_isodata_write.values
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/lib/filematch.sh
    "${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/isodata/isodata_write.values.ref"
    "isodata_write.values.test"
)
set_tests_properties(
    libgrasp_isodata_write.zeros libgrasp_isodata_write.values
    PROPERTIES
    DEPENDS libgrasp_isodata_write
)

# Check lib92 routines -- this currently acts as a simple integration test,
# making sure that everything actually got compiled properly. The test checks
# that the QUAD routine from lib92 actually produces reasonable numbers.
add_executable(test.lib9290_quad
    lib9290_quad.f90
)
target_link_libraries_Fortran(test.lib9290_quad PUBLIC mod 9290)
add_test(lib9290_quad test.lib9290_quad)
