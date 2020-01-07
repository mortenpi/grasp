!> Tests that derived constants have sane value.
program libgrasp_orbitals_write
   use, intrinsic :: iso_fortran_env, only: real64
   use grasptest, only: test_isequal, abstol, getenv_allocating
   use grasp_datafiles, only: orbital, orbitals_read, orbitals_write
   implicit none
   integer, parameter :: dp = real64
   ! Local variables
   logical :: status, success = .true.
   type(orbital), allocatable, dimension(:) :: orbitals, orbitals_in
   integer :: fileunit, ios, k
   character(255) :: iom
   character(:), allocatable :: srcdir

   status = getenv_allocating('GRASP_ORBITALS_SOURCE', srcdir)
   if (.not. status) then
      print *, "Failed to get GRASP_ORBITALS_SOURCE environment variable"
      error stop
   endif

   ! An empty list of orbitals (edge case)
   allocate(orbitals(0))
   call test_isequal(success, '(size=0):size', size(orbitals), 0)
   status = orbitals_write(orbitals, "rwfn.0")
   call test_isequal(success, '(size=0):status', status, .true.)
   ! Read the file back in
   status = orbitals_read("rwfn.0", orbitals_in)
   call test_isequal(success, '(size=0):in:status', status, .true.)
   call test_isequal(success, '(size=0):in:size', size(orbitals_in), 0)
   ! Try writing again -- should fail, since file exists
   status = orbitals_write(orbitals, "rwfn.0")
   call test_isequal(success, '(size=0):status', status, .false.)
   call delete_file("rwfn.0")
   deallocate(orbitals_in)
   deallocate(orbitals)

   ! A single orbital (edge case)
   allocate(orbitals(1))
   call test_isequal(success, '(size=1):size', size(orbitals), 1)
   orbitals(1)%n = 10
   orbitals(1)%kappa = -5
   orbitals(1)%e = 1.2_dp
   orbitals(1)%npts = 10
   allocate(orbitals(1)%p(10))
   allocate(orbitals(1)%q(10))
   allocate(orbitals(1)%r(10))
   do k = 1, 10
      orbitals(1)%r(k) = k / 10_dp
      orbitals(1)%p(k) = k
      orbitals(1)%q(k) = -2*k
   enddo
   status = orbitals_write(orbitals, "rwfn.1")
   call test_isequal(success, '(size=1):status', status, .true.)
   ! Read the file back in
   status = orbitals_read("rwfn.1", orbitals_in)
   call test_isequal(success, '(size=1):in:status', status, .true.)
   call test_isequal(success, '(size=1):in:size', size(orbitals_in), 1)
   call test_isequal(success, '(size=1):in:n', orbitals_in(1)%n, 10)
   call test_isequal(success, '(size=1):in:kappa', orbitals_in(1)%kappa, -5)
   call test_isequal(success, '(size=1):in:e', orbitals_in(1)%e, 1.2_dp, 1e-10_dp)
   call test_isequal(success, '(size=1):in:q(5)', orbitals_in(1)%q(5), -10.0_dp, 1e-10_dp)
   ! Cleanup
   call delete_file("rwfn.1")
   deallocate(orbitals_in)
   deallocate(orbitals)

   ! Several orbitals
   allocate(orbitals(199))
   do k = 1, size(orbitals)
      orbitals(k)%n = k
      orbitals(k)%kappa = -k
      orbitals(k)%npts = 100*k
      allocate(orbitals(k)%p(orbitals(k)%npts))
      allocate(orbitals(k)%q(orbitals(k)%npts))
      allocate(orbitals(k)%r(orbitals(k)%npts))
   enddo
   status = orbitals_write(orbitals, "rwfn.3")
   call test_isequal(success, '(size=3):status', status, .true.)
   ! Read the file back in
   call test_isequal(success, '(size=3):in:size', allocated(orbitals_in), .false.)
   status = orbitals_read("rwfn.3", orbitals_in)
   call test_isequal(success, '(size=3):in:status', status, .true.)
   call test_isequal(success, '(size=3):in:size', size(orbitals_in), size(orbitals))
   do k = 1, size(orbitals)
      call test_isequal(success, '(size=3):in(k):n', orbitals_in(k)%n, k)
      call test_isequal(success, '(size=3):in(k):kappa', orbitals_in(k)%kappa, -k)
      call test_isequal(success, '(size=3):in(k):npts', orbitals_in(k)%npts, 100*k)
      call test_isequal(success, '(size=3):in(k):size(p)', size(orbitals_in(k)%p), 100*k)
      call test_isequal(success, '(size=3):in(k):size(q)', size(orbitals_in(k)%q), 100*k)
      call test_isequal(success, '(size=3):in(k):size(r)', size(orbitals_in(k)%r), 100*k)
   enddo
   ! Cleanup
   call delete_file("rwfn.3")
   deallocate(orbitals_in)

   ! Try writing to a (hopefully) prohibited file
   status = orbitals_write(orbitals, "/root/foo/bar/baz")
   call test_isequal(success, 'prohibited:', status, .false.)
   deallocate(orbitals)

   ! Read in the rwfnestimate file and then write it out again
   status = orbitals_read(srcdir//"/rwfn.inp", orbitals_in)
   call test_isequal(success, 'rwfn:status', status, .true.)
   call test_isequal(success, 'rwfn:allocated', allocated(orbitals_in), .true.)
   call test_isequal(success, 'rwfn:length', size(orbitals_in), 25)
   ! Write it out. We'll check that this file matches the input file bit by bit in a separate
   ! scripts
   status = orbitals_write(orbitals_in, "rwfn.out")
   deallocate(orbitals_in)

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

end program libgrasp_orbitals_write
