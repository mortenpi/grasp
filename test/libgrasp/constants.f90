!> Tests that derived constants have sane value.
program libgrasp_constants
   use, intrinsic :: iso_fortran_env, only: real64
   use grasptest, only: test_isequal
   use grasp_constants, only: fm
   implicit none
   integer, parameter :: dp = real64

   logical :: success = .true.

   call test_isequal(success, 'fm in a.u.', fm, 1.889726125e-5_dp, 1e-9_dp)

   if (.not. success) then
      print *, "libgrasp_constants: Tests failed."
      stop 1
   end if

end program libgrasp_constants
