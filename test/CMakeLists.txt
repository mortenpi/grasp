# Make sure the test executables end up on build/test/, not build/bin/
set(BUILD_BINDIR ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
unset(CMAKE_RUNTIME_OUTPUT_DIRECTORY)

# Let's catch implicit uses of routines in the test prorams and libraries
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Werror=implicit-interface -Werror=implicit-procedure")

# A library of test helper routines etc.
add_library(grasptest STATIC
    lib/grasptest.f90
)
setup_fortran_modules(grasptest)

# Tests for libgrasp
# ==========================================================================================
# libgrasp constants
add_executable(test.libgrasp_constants libgrasp/constants.f90)
target_link_libraries_Fortran(test.libgrasp_constants PUBLIC grasp-static grasptest)
add_test(libgrasp_constants test.libgrasp_constants)
# libgrasp constants (but with shared library)
add_executable(test.libgrasp_constants.shared libgrasp/constants.f90)
target_link_libraries_Fortran(test.libgrasp_constants.shared PUBLIC grasp grasptest)
add_test(libgrasp_constants.shared test.libgrasp_constants.shared)

# Reading and writing isodata files
# ---------------------------------
# Reading isodata files
add_executable(test.libgrasp_isodata_read libgrasp/isodata/read.f90)
target_link_libraries_Fortran(test.libgrasp_isodata_read PUBLIC grasp-static grasptest)
add_test(libgrasp_isodata_read test.libgrasp_isodata_read)
set_property(TEST libgrasp_isodata_read PROPERTY ENVIRONMENT
    "GRASP_ISODATA_SOURCE=${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/isodata"
)
# Writing isodata files
add_executable(test.libgrasp_isodata_write libgrasp/isodata/write.f90)
target_link_libraries_Fortran(test.libgrasp_isodata_write PUBLIC grasp-static grasptest)
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

# Orbital files
# -------------
add_test(NAME libgrasp_orbitals_generate
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/orbitals/generate.sh
)
set_property(TEST libgrasp_orbitals_generate PROPERTY ENVIRONMENT
    "GRASP_BINDIR=${BUILD_BINDIR}"
)
set_tests_properties(libgrasp_orbitals_generate PROPERTIES FIXTURES_SETUP libgrasp_orbitals_generate.fixture)
add_test(NAME libgrasp_orbitals_generate.cleanup
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/orbitals/generate.sh
    "cleanup"
)
set_tests_properties(libgrasp_orbitals_generate.cleanup PROPERTIES FIXTURES_CLEANUP libgrasp_orbitals_generate.fixture)
# Reading orbital files
add_executable(test.libgrasp_orbitals_read libgrasp/orbitals/read.f90)
target_link_libraries_Fortran(test.libgrasp_orbitals_read PUBLIC grasp-static grasptest)
add_test(libgrasp_orbitals_read test.libgrasp_orbitals_read)
set_property(TEST libgrasp_orbitals_read PROPERTY ENVIRONMENT
    "GRASP_ORBITALS_SOURCE=${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/orbitals"
)
set_tests_properties(
    libgrasp_orbitals_read
    PROPERTIES
    FIXTURES_REQUIRED libgrasp_orbitals_generate.fixture
)

# Writing orbital files
add_executable(test.libgrasp_orbitals_write libgrasp/orbitals/write.f90)
target_link_libraries_Fortran(test.libgrasp_orbitals_write PUBLIC grasp-static grasptest)
add_test(libgrasp_orbitals_write test.libgrasp_orbitals_write)
set_tests_properties(libgrasp_orbitals_write PROPERTIES
    FIXTURES_REQUIRED "libgrasp_orbitals_write.fixture;libgrasp_orbitals_generate.fixture"
    FIXTURES_SETUP libgrasp_orbitals_write.checkoutput.fixture
    ENVIRONMENT "GRASP_ORBITALS_SOURCE=${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/orbitals"
)
# Check the output rwfn file
add_test(NAME libgrasp_orbitals_write.checkoutput COMMAND diff "rwfn.out" "${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/orbitals/rwfn.inp")
set_tests_properties(libgrasp_orbitals_write.checkoutput PROPERTIES
    FIXTURES_REQUIRED "libgrasp_orbitals_write.checkoutput.fixture;libgrasp_orbitals_write.fixture;libgrasp_orbitals_generate.fixture"
)
# Cleanup
add_test(NAME libgrasp_orbitals_write.setup COMMAND sh -c "rm -vf rwfn.*")
set_tests_properties(libgrasp_orbitals_write.setup PROPERTIES FIXTURES_SETUP libgrasp_orbitals_write.fixture)
add_test(NAME libgrasp_orbitals_write.cleanup COMMAND sh -c "rm -vf rwfn.*")
set_tests_properties(libgrasp_orbitals_write.cleanup PROPERTIES FIXTURES_CLEANUP libgrasp_orbitals_write.fixture)

# ASF files
# ---------
add_test(NAME libgrasp_asfs_generate
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/asfs/generate.sh
)
set_property(TEST libgrasp_asfs_generate PROPERTY ENVIRONMENT
    "GRASP_BINDIR=${BUILD_BINDIR}"
)
set_tests_properties(libgrasp_asfs_generate PROPERTIES FIXTURES_SETUP libgrasp_asfs_generate.fixture)
add_test(NAME libgrasp_asfs_generate.cleanup
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/asfs/generate.sh
    "cleanup"
)
set_tests_properties(libgrasp_asfs_generate.cleanup PROPERTIES FIXTURES_CLEANUP libgrasp_asfs_generate.fixture)
# Reading and writing ASF files
add_executable(test.libgrasp_asfs_readwrite libgrasp/asfs/readwrite.f90)
target_link_libraries_Fortran(test.libgrasp_asfs_readwrite PUBLIC grasp-static grasptest)
add_test(libgrasp_asfs_readwrite test.libgrasp_asfs_readwrite)
set_tests_properties(
    libgrasp_asfs_readwrite
    PROPERTIES
    FIXTURES_REQUIRED "libgrasp_asfs_generate.fixture;libgrasp_asfs_readwrite.fixture"
    FIXTURES_SETUP libgrasp_asfs_readwrite.checkoutput.fixture
    ENVIRONMENT "GRASP_ASFS_SOURCE=${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/asfs"
)
# Check written file
add_test(NAME libgrasp_asfs_readwrite.checkoutput COMMAND diff "teststate.m" "${CMAKE_CURRENT_SOURCE_DIR}/libgrasp/asfs/teststate.cm")
set_tests_properties(libgrasp_asfs_readwrite.checkoutput PROPERTIES
    FIXTURES_REQUIRED "libgrasp_asfs_readwrite.checkoutput.fixture;libgrasp_asfs_readwrite.fixture;libgrasp_asfs_generate.fixture"
)
# Cleanup
add_test(NAME libgrasp_asfs_readwrite.setup COMMAND sh -c "rm -vf teststate.*")
set_tests_properties(libgrasp_asfs_readwrite.setup PROPERTIES FIXTURES_SETUP libgrasp_asfs_readwrite.fixture)
add_test(NAME libgrasp_asfs_readwrite.cleanup COMMAND sh -c "rm -vf teststate.*")
set_tests_properties(libgrasp_asfs_readwrite.cleanup PROPERTIES FIXTURES_CLEANUP libgrasp_asfs_readwrite.fixture)

# Check lib92 routines -- this currently acts as a simple integration test,
# making sure that everything actually got compiled properly. The test checks
# that the QUAD routine from lib92 actually produces reasonable numbers.
add_executable(test.lib9290_quad
    lib9290_quad.f90
)
target_link_libraries_Fortran(test.lib9290_quad PUBLIC mod 9290)
add_test(lib9290_quad test.lib9290_quad)
