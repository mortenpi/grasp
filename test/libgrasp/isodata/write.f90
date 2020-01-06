!> Tests that derived constants have sane value.
program libgrasp_isodata_write
   use, intrinsic :: iso_fortran_env, only: real64
   use grasptest, only: test_isequal, abstol, getenv_allocating
   use grasp_datafiles, only: isodata, isodata_read, isodata_write
   implicit none
   integer, parameter :: dp = real64
   ! Local variables
   logical :: success = .true.
   type(isodata) :: iso, isoin
   integer :: fileunit, ios
   character(255) :: iom
   logical status

   ! All zeros
   iso%z = 0.0_dp
   iso%a = 0.0_dp
   iso%mass = 0.0_dp
   iso%fermi_a = 0.0_dp
   iso%fermi_c = 0.0_dp
   iso%spin = 0.0_dp
   iso%dipolemoment = 0.0_dp
   iso%quadrupolemoment = 0.0_dp
   call delete_file('isodata_write.zeros.test') ! delete output file, if exists
   status = isodata_write(iso, "isodata_write.zeros.test")
   call test_isequal(success, 'zeros:write', status, .true.)

   open(newunit=fileunit, file='isodata_write.zeros.test', status='old', iostat=ios, iomsg=iom)
   if(ios /= 0) then
      print *, "ERROR: Unable to open file:", ios, iom
      success = .false.
      return
   else
      status = isodata_read(fileunit, isoin)
      call test_isequal(success, 'zeros:read', status, .true.)
      call test_isequal(success, 'zeros:read:Z', isoin%z, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:A', isoin%a, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:M', isoin%mass, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:FA', isoin%fermi_a, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:FC', isoin%fermi_c, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:S', isoin%spin, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:DP', isoin%dipolemoment, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'zeros:read:QP', isoin%quadrupolemoment, 0.0_dp, abstol(1e-15_dp))
      close(fileunit)
   endif

   ! All values
   iso%z = 1.0_dp
   iso%a = 3.0_dp
   iso%mass = 3.0155006200_dp
   iso%fermi_a = 0.5_dp
   iso%fermi_c = 1.4e-3_dp
   iso%spin = 1.5_dp
   iso%dipolemoment = 0.0_dp
   iso%quadrupolemoment = 5.0_dp
   call delete_file('isodata_write.values.test') ! delete output file, if exists
   status = isodata_write(iso, "isodata_write.values.test")
   call test_isequal(success, 'values:write', status, .true.)

   open(newunit=fileunit, file='isodata_write.values.test', status='old', iostat=ios, iomsg=iom)
   if(ios /= 0) then
      print *, "ERROR: Unable to open file:", ios, iom
      success = .false.
      return
   else
      status = isodata_read(fileunit, isoin)
      call test_isequal(success, 'values:read', status, .true.)
      call test_isequal(success, 'values:write:Z', isoin%z, 1.0_dp, 1e-15_dp)
      call test_isequal(success, 'values:write:A', isoin%a, 3.0_dp, 1e-15_dp)
      call test_isequal(success, 'values:write:M', isoin%mass, 3.0155006200_dp, 1e-15_dp)
      call test_isequal(success, 'values:write:FA', isoin%fermi_a, 0.5_dp, 1e-15_dp)
      call test_isequal(success, 'values:write:FC', isoin%fermi_c, 1.4e-3_dp, 1e-15_dp)
      call test_isequal(success, 'values:write:S', isoin%spin, 1.5_dp, 1e-15_dp)
      call test_isequal(success, 'values:write:DP', isoin%dipolemoment, 0.0_dp, abstol(1e-15_dp))
      call test_isequal(success, 'values:write:QP', isoin%quadrupolemoment, 5.0_dp, 1e-15_dp)
      close(fileunit)
   endif

   if (.not. success) then
      print *, "libgrasp_isodata_write: Tests failed."
      stop 1
   end if

contains

   subroutine delete_file(filename)
      character(len=*), intent(in) :: filename
      ! Local variables
      integer :: fileunit, ios
      open(newunit=fileunit, file=filename, iostat=ios, status='old')
      if(ios == 0) close(fileunit, status='delete')
   end subroutine delete_file

end program libgrasp_isodata_write
