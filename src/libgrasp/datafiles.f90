!> Contains types and routines for reading and writing GRASP data files (such
!! as mixing coefficients, orbitals etc.).
module grasp_datafiles
   use, intrinsic :: iso_fortran_env, only: real64, int32
   implicit none

   type :: blocksinfo_t
      integer(int32) :: nblocks
      integer(int32) :: nelectrons
      integer(int32) :: ncsfstotal
      integer(int32) :: norbitals
      integer(int32) :: nvectotal
      integer(int32) :: nvecsize
   end type

   type :: block_t
      integer(int32) :: blockid
      integer(int32) :: ncsfs
      integer(int32) :: nevs
      integer(int32) :: iatjp, iaspa
      real(real64) :: eav
      !type(c_ptr) :: eigenstates
      !type(c_ptr) :: eigenenergies
   end type

   type isodata
      real(real64) :: z
      real(real64) :: a
      real(real64) :: mass
      integer(int32) :: nuclearmodel
      real(real64) :: nucleus(2)
      !...
      real(real64) :: SQN, DMOMNM, QMOMB
   end type isodata

   ! TODO: compiler option for big/small endian

contains

   !> ...
   !!
   !! @return ...
   function isodata_read(filename, iso)
      character(len=*), intent(in) :: filename
      type(isodata), intent(out) :: iso
      logical :: isodata_read
      ! Local variables
      integer :: fhandle, ios
      character(255) :: iom
      character(6) :: g92mix

      real(real64) :: fermi_a, fermi_c

      open (newunit=fhandle, file=filename, form="unformatted", status="old", iostat=ios, iomsg=iom)
      if (ios /= 0) then
         print *, "ERROR: Unable to open file:", ios, iom
         isodata_read = 1
         return
      endif

      ! Check header
      read (fhandle, iostat=ios, iomsg=iom) g92mix
      if (ios /= 0) then
         print *, "ERROR: Unable to read from file:", ios, iom
         isodata_read = 1
         return
      endif
      if (g92mix /= "G92MIX") then
         print *, "ERROR: Bad file header -- not a G92MIX file?"
         isodata_read = 3
         return
      endif

      ! Read the isodata file
      read (22, *) iso%z
      read (22, *)
      read (22, *) iso%a
      read (22, *)
      read (22, *) fermi_a
      read (22, *)
      read (22, *) fermi_c
      read (22, *)
      read (22, *) nuclearmass
      read (22, *)
      read (22, *) iso%SQN
      read (22, *)
      read (22, *) iso%DMOMNM
      read (22, *)
      read (22, *) iso%QMOMB

      if (iso%a /= 0.0_dp) then
         iso%nuclearmodel = 2
         iso%nucleus(1) = fermi_c*fm2au
         iso%nucleus(2) = fermi_a*fm2au
      else
         iso%nuclearmodel = 0
      endif
   end

end module grasp_datafiles
