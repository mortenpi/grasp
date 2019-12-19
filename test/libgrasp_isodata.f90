!> Tests that derived constants have sane value.
program libgrasp_isodata
   use, intrinsic :: iso_fortran_env, only: real64
   use grasptest, only: test_isequal, abstol, getenv_allocating
   use grasp_datafiles, only: isodata, isodata_read
   implicit none
   integer, parameter :: dp = real64
   ! Local variables
   logical :: success = .true.
   type(isodata) :: iso
   logical status
   character(:), allocatable :: isodatasrc

   status = getenv_allocating('GRASP_ISODATA_SOURCE', isodatasrc)
   if (.not. status) then
      print *, "Failed to get GRASP_ISODATA_SOURCE environment variable"
      error stop
   endif
   print *, isodatasrc//"/H1_point"

   ! Check error handling. This file should not exist.
   status = isodata_read('/foo/bar/baz', iso)
   call test_isequal(success, 'isodata_read:error', status, .false.)
   ! This file exists, but should contain rubbish
   status = isodata_read(isodatasrc//"/rubbish", iso)
   call test_isequal(success, 'isodata_read:rubbish', status, .false.)

   ! Point nucleus
   status = isodata_read(isodatasrc//"/H1_point", iso)
   call test_isequal(success, 'isodata_read:H1_point', status, .true.)
   call test_isequal(success, 'isodata_read:H1_point:Z', iso%z, 1.0_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H1_point:A', iso%a, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H1_point:M', iso%mass, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H1_point:FA', iso%fermi_a, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H1_point:FC', iso%fermi_c, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H1_point:S', iso%spin, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H1_point:DP', iso%dipolemoment, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H1_point:QP', iso%quadrupolemoment, 0.0_dp, abstol(1e-15_dp))

   ! Fermi nucleus
   status = isodata_read(isodatasrc//"/H3_finite", iso)
   call test_isequal(success, 'isodata_read:H3_finite', status, .true.)
   call test_isequal(success, 'isodata_read:H3_finite:Z', iso%z, 1.0_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H3_finite:A', iso%a, 3.0_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H3_finite:M', iso%mass, 3.0155006200969998_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H3_finite:FA', iso%fermi_a, 0.52338755531043146_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H3_finite:FC', iso%fermi_c, 1.4865427022810927e-103_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H3_finite:S', iso%spin, 1.0_dp, 1e-15_dp)
   call test_isequal(success, 'isodata_read:H3_finite:DP', iso%dipolemoment, 0.0_dp, abstol(1e-15_dp))
   call test_isequal(success, 'isodata_read:H3_finite:QP', iso%quadrupolemoment, 0.0_dp, abstol(1e-15_dp))

   if (.not. success) then
      print *, "libgrasp_isodata: Tests failed."
      stop 1
   end if

end program libgrasp_isodata
