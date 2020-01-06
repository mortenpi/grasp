!> Tests that derived constants have sane value.
program libgrasp_orbitals_read
   use, intrinsic :: iso_fortran_env, only: real64
   use grasptest, only: test_isequal, abstol, getenv_allocating
   use grasp_datafiles, only: orbital, orbitals_read
   implicit none
   integer, parameter :: dp = real64
   ! Local variables
   logical :: success = .true.
   type(orbital), allocatable, dimension(:) :: orbitals
   logical status
   character(:), allocatable :: sourcedirectory

   status = getenv_allocating('GRASP_ORBITALS_SOURCE', sourcedirectory)
   if (.not. status) then
      print *, "Failed to get GRASP_ORBITALS_SOURCE environment variable"
      error stop
   endif
   print *, sourcedirectory//"/rwfn.inp"

   ! Check error handling. This file should not exist.
   status = orbitals_read('/foo/bar/baz', orbitals)
   call test_isequal(success, 'orbitals_read:error', status, .false.)
   ! This file exists, but should contain rubbish
   status = orbitals_read(sourcedirectory//"/rubbish", orbitals)
   call test_isequal(success, 'orbitals_read:rubbish', status, .false.)

   ! rwfn.inp file generated by the generate.sh script
   call test_isequal(success, 'orbitals_read:rwfn:deallocated', allocated(orbitals), .false.)
   status = orbitals_read(sourcedirectory//"/rwfn.inp", orbitals)
   call test_isequal(success, 'orbitals_read:rwfn', status, .true.)
   call test_isequal(success, 'orbitals_read:rwfn:allocated', allocated(orbitals), .true.)
   call test_isequal(success, 'orbitals_read:rwfn:length', size(orbitals), 25)
   ! #1: 1s
   call test_isequal(success, 'orbitals_read:rwfn:os(1):n', orbitals(1)%n, 1)
   call test_isequal(success, 'orbitals_read:rwfn:os(1):n', orbitals(1)%kappa, -1)
   ! #2: 2s
   call test_isequal(success, 'orbitals_read:rwfn:os(2):n', orbitals(2)%n, 2)
   call test_isequal(success, 'orbitals_read:rwfn:os(2):n', orbitals(2)%kappa, -1)
   ! #3: 2p-
   call test_isequal(success, 'orbitals_read:rwfn:os(3):n', orbitals(3)%n, 2)
   call test_isequal(success, 'orbitals_read:rwfn:os(3):n', orbitals(3)%kappa, 1)
   ! #4: 2p
   call test_isequal(success, 'orbitals_read:rwfn:os(4):n', orbitals(4)%n, 2)
   call test_isequal(success, 'orbitals_read:rwfn:os(4):n', orbitals(4)%kappa, -2)
   ! #8: 3d-
   call test_isequal(success, 'orbitals_read:rwfn:os(8):n', orbitals(8)%n, 3)
   call test_isequal(success, 'orbitals_read:rwfn:os(8):n', orbitals(8)%kappa, 2)
   ! #9: 3d
   call test_isequal(success, 'orbitals_read:rwfn:os(9):n', orbitals(9)%n, 3)
   call test_isequal(success, 'orbitals_read:rwfn:os(9):n', orbitals(9)%kappa, -3)
   ! #23: 6p-
   call test_isequal(success, 'orbitals_read:rwfn:os(23):n', orbitals(23)%n, 6)
   call test_isequal(success, 'orbitals_read:rwfn:os(23):n', orbitals(23)%kappa, 1)
   ! #24: 6p
   call test_isequal(success, 'orbitals_read:rwfn:os(24):n', orbitals(24)%n, 6)
   call test_isequal(success, 'orbitals_read:rwfn:os(24):n', orbitals(24)%kappa, -2)
   ! #25: 7s
   call test_isequal(success, 'orbitals_read:rwfn:os(25):n', orbitals(25)%n, 7)
   call test_isequal(success, 'orbitals_read:rwfn:os(25):n', orbitals(25)%kappa, -1)

   if (.not. success) then
      print *, "libgrasp_isodata: Tests failed."
      stop 1
   end if

end program libgrasp_orbitals_read
