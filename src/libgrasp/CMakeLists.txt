add_library(grasp-static STATIC
    constants.f90
    datafiles.f90
)
setup_fortran_modules(grasp-static)

add_library(grasp SHARED
    constants.f90
    datafiles.f90
)
setup_fortran_modules(grasp)
install(TARGETS grasp DESTINATION lib/)
