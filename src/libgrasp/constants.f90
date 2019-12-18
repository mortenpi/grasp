!> Library of physical constants.
module grasp_constants
   use, intrinsic :: iso_fortran_env, only: real64
   implicit none
   integer, parameter :: dp = real64

   !> Bohr radius (unit of length in atomic units) in centimetres.
   ! NOTE: CODATA 2018 value, 5.291 772 109 03 (80) × 10^-11 m, is different
   ! from this. This value is from the SETCON routine.
   real(real64), parameter :: a_cm = 0.52917721067e-8_dp

   !> `1 fm` in atomic units
   real(real64), parameter :: fm = 1e-13_dp/a_cm

end module grasp_constants
