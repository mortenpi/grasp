!> Various handy routines useful for writing unit tests.
module grasptest
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none

   interface test_isequal
      module procedure test_isequal_real64, test_isequal_abstol_real64
      module procedure test_isequal_integer
      module procedure test_isequal_logical
   end interface test_isequal

   !> Wrapper type for representing an absolute tolerance value.
   !!
   !! This wrapper type is necessary so that we could dispatch with module in
   !! the following way:
   !!
   !! ```
   !! ! Test x == 1.0 with relative tolerance:
   !! call test_isequal(success, 'reltest', x, 1.0_dp, 1e-15)
   !! ! Test y == 0.0 with absolute tolerance:
   !! call test_isequal(success, 'reltest', x, y.0_dp, abstol(1e-15))
   !! ```
   type abstol
      real(real64) :: tol
   end type abstol

contains

   !> Calculate the relative difference of `a` and `b`.
   !!
   !! The relative difference is defined as:
   !!
   !! \f[
   !!   \frac{|a-b|}{\max(|a|, |b|)}
   !! \f]
   !!
   !! @param a,b Input values.
   !! @returns The relative difference of `a` and `b`.
   function reldiff(a, b)
      real(real64), intent(in) :: a, b
      real(real64) :: reldiff
      reldiff = abs(a - b)/max(abs(a), abs(b))
   end function reldiff

   !> Checks if the difference of `a` and `b` are within the tolerance relative
   !! to \f$\max(|a|,|b|)\f$.
   !!
   !! @param a,b Values to be checked.
   !! @param relative_tolerance Relative tolerance \f$\sigma\f$.
   !! @returns Whether \f$|a-b| / \max(|a|,|b|) < \sigma\f$.
   function within_tolerance(a, b, relative_tolerance)
      real(real64), intent(in) :: a, b, relative_tolerance
      logical :: within_tolerance

      if (reldiff(a, b) < relative_tolerance) then
         within_tolerance = .true.
      else
         within_tolerance = .false.
      endif
   end function within_tolerance

   !> Tests if two floating point values are the same within the specified
   !! relative tolerance.
   !!
   !! Specifically, it tests that
   !!
   !! \f[
   !!     \frac{|a-b|}{\max(|a|, |b|)} < \sigma
   !! \f]
   !!
   !! where \f$\sigma\f$ is the specified relative tolerance.
   !!
   !! If the test fails, `test_passed` gets set to `.false.`, but the value is
   !! unchanged if the tests pass. This allows for the pattern where the test
   !! function is called multiple times before checking if any of the tests
   !! failed:
   !!
   !! ```
   !! call test_equality(success, "T1", a1, b1, rtol)
   !! call test_equality(success, "T2", a2, b2, rtol)
   !! if(.not.success) then
   !!     print *, "Test failures occurred."
   !! endif
   !! ```
   !!
   !! On failure, a message gets printed into the standard output that a test
   !! failed, which also includes the `which` string, allowing for the failed
   !! test to be identified easily.
   !!
   !! @param test_passed Gets set to `.false.` if the test fails, and is left
   !!   unchanged otherwise.
   !! @param which A string that identifies the test.
   !! @param a,b Values to be compared.
   !! @param relative_tolerance The relative tolerance \f$\sigma\f$.
   subroutine test_isequal_real64(test_passed, which, a, b, relative_tolerance)
      logical, intent(inout) :: test_passed
      character(*), intent(in) :: which
      real(real64), intent(in) :: a, b, relative_tolerance
      real(real64) :: relative_difference

      if (.not. within_tolerance(a, b, relative_tolerance)) then
         relative_difference = abs(a - b)/max(abs(a), abs(b))
         print '("  Test failed: ",a," not within tolerance. Rel.diff: ",es12.5,", tol: ",es12.5)', &
            which, relative_difference, relative_tolerance
         test_passed = .false.
      endif
   end subroutine test_isequal_real64

   !> Checks if the difference of `a` and `b` are equal (within absolute
   !! tolerance).
   !!
   !! @param a,b Values to be checked.
   !! @param absolute_tolerance Absolute tolerance \f$\sigma\f$.
   !! @returns Whether \f$|a-b| < \sigma\f$.
   function within_abstol(a, b, absolute_tolerance)
      real(real64), intent(in) :: a, b
      type(abstol), intent(in) :: absolute_tolerance
      logical :: within_abstol

      if (abs(a - b) < absolute_tolerance%tol) then
         within_abstol = .true.
      else
         within_abstol = .false.
      endif
   end function within_abstol

   !> Tests if two floating point values are equal within the specified absolute
   !! tolerance.
   !!
   !! Specifically, it tests that
   !!
   !! \f[
   !!     |a-b| < \sigma
   !! \f]
   !!
   !! where \f$\sigma\f$ is the specified absolute tolerance.
   !!
   !! If the test fails, `test_passed` gets set to `.false.`, but the value is
   !! unchanged if the tests pass. This allows for the pattern where the test
   !! function is called multiple times before checking if any of the tests
   !! failed:
   !!
   !! ```
   !! call test_equality(success, "T1", a1, b1, abstol(atol))
   !! call test_equality(success, "T2", a2, b2, abstol(atol))
   !! if(.not.success) then
   !!     print *, "Test failures occurred."
   !! endif
   !! ```
   !!
   !! On failure, a message gets printed into the standard output that a test
   !! failed, which also includes the `which` string, allowing for the failed
   !! test to be identified easily.
   !!
   !! @param test_passed Gets set to `.false.` if the test fails, and is left
   !!   unchanged otherwise.
   !! @param which A string that identifies the test.
   !! @param a,b Values to be compared.
   !! @param absolute_tolerance The absolute tolerance \f$\sigma\f$, passed as
   !!   instance of the `abstol` wrapper type.
   subroutine test_isequal_abstol_real64(test_passed, which, a, b, absolute_tolerance)
      logical, intent(inout) :: test_passed
      character(*), intent(in) :: which
      real(real64), intent(in) :: a, b
      type(abstol), intent(in) :: absolute_tolerance

      if (.not. within_abstol(a, b, absolute_tolerance)) then
         print '("  Test failed: ",a," not within abs.tolerance. Diff: ",es12.5,", tol: ",es12.5)', &
            which, abs(a - b), absolute_tolerance%tol
         test_passed = .false.
      endif
   end subroutine test_isequal_abstol_real64

   !> Tests if two logical values are equal.
   subroutine test_isequal_integer(test_passed, which, a, b)
      logical, intent(inout) :: test_passed
      character(*), intent(in) :: which
      integer, intent(in) :: a, b

      if (a /= b) then
         print '("  Test failed: integers values differ for ",a," (a=",I0,", b=",I0,")")', &
            which, a, b
         test_passed = .false.
      endif
   end subroutine test_isequal_integer

   !> Tests if two logical values are equal.
   subroutine test_isequal_logical(test_passed, which, a, b)
      logical, intent(inout) :: test_passed
      character(*), intent(in) :: which
      logical, intent(in) :: a, b

      if (.not. (a .eqv. b)) then
         print '("  Test failed: logical values differ for ",a," (a=",L1,", b=",L1,")")', &
            which, a, b
         test_passed = .false.
      endif
   end subroutine test_isequal_logical

   !> Fetches an environment variable.
   !!
   !! If it was able to fetch the variable value, returns `.true.` and sets
   !! `value` to the value ([re]allocating if necessary). Returns `.false.` if
   !! the variable is not defined
   !!
   !! Under the hood it calls `get_environment_variable`, but it properly
   !! allocates or re-allocates the `value` to match the actual length of the
   !! environment variable.
   !!
   !! TODO: For this to be a proper library function, it should be implemented
   !! as an interface with additional methods to handle pointers and fixed-length
   !! strings.
   function getenv_allocating(variable_name, value)
      logical :: getenv_allocating
      character(*), intent(in) :: variable_name
      character(:), allocatable, intent(inout) :: value
      integer :: length, status
      character(1) :: test

      ! First, make an inquiry call to get_environment_variable to determine
      ! whether the variable exists and, if so, its length.
      call get_environment_variable(variable_name, test, length, status)
      if (status > 0) then
         ! status of 1 or 2 from get_environment_variable means that the the
         ! variable is undefined, or that the system does not support
         ! environment variables at all, respectively. Both situations make
         ! getenv fail.
         getenv_allocating = .false.
         return
      endif
      ! Will allocate or re-allocate value, unless it already has the correct length.
      if (allocated(value) .and. len(value) /= length) deallocate (value)
      if (.not. allocated(value)) allocate (character(length) :: value)
      ! Can't pass a length 0 string to get_environment_variable.
      if (length > 0) then
         call get_environment_variable(variable_name, value, length, status)
      endif
      getenv_allocating = .true.
   end

end module grasptest
