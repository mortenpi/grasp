# Make sure the test executables end up on build/test/, not build/bin/
unset(CMAKE_RUNTIME_OUTPUT_DIRECTORY)

# Let's catch implicit uses of routines in the test prorams and libraries
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Werror=implicit-interface -Werror=implicit-procedure")

# A library of test helper routines etc.
add_library(grasptest STATIC
    lib/grasptest.f90
)
setup_fortran_modules(grasptest)

# Check lib92 routines -- this currently acts as a simple integration test,
# making sure that everything actually got compiled properly. The test checks
# that the QUAD routine from lib92 actually produces reasonable numbers.
add_executable(test.libgrasp_constants
    libgrasp_constants.f90
)
target_link_libraries_Fortran(test.libgrasp_constants PUBLIC grasp grasptest)
add_test(libgrasp_constants test.libgrasp_constants)

# Check lib92 routines -- this currently acts as a simple integration test,
# making sure that everything actually got compiled properly. The test checks
# that the QUAD routine from lib92 actually produces reasonable numbers.
add_executable(test.lib9290_quad
    lib9290_quad.f90
)
target_link_libraries_Fortran(test.lib9290_quad PUBLIC mod 9290)
add_test(lib9290_quad test.lib9290_quad)
