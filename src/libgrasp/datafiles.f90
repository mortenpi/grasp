!> Contains types and routines for reading and writing GRASP data files (such
!! as mixing coefficients, orbitals etc.).
module grasp_datafiles
   use, intrinsic :: iso_fortran_env, only: real64, int32
   implicit none

   !> A data structure representing the contents of an `isodata` file,
   !! containing nuclear information.
   !!
   !! If the nuclear mass `mass` is set to zero, the nucleus is assumed to be
   !! a point nucleus and the distribution parameters `nucleus` are ignored.
   !!
   !! If the nuclear mass is non-zero, the nuclear model is assumed to be the
   !! two-parameter Fermi distribution
   !!
   !! \f[
   !!   \rho(r) = \frac{\rho_{0}}{1 + \mathrm{e}^{(r-c)/a}}
   !! \f]
   !!
   !! with the \f$a\f$ and \f$c\f$ parameters given by `fermi_a` and `fermi_c`,
   !! respectively.
   type isodata
      !> Nuclear charge \f$Z\f$.
      real(real64) :: z
      !> Atomic mass number.
      real(real64) :: a
      !> Nuclear mass in atomic mass units.
      real(real64) :: mass
      !> The \f$a\f$ parameter of the Fermi charge distribution, in femtomenter.
      !! The value is meaningful only if the `mass /= 0`.
      real(real64) :: fermi_a
      !> The \f$c\f$ parameter of the Fermi charge distribution, in femtomenter.
      !! The value is meaningful only if the `mass /= 0`.
      real(real64) :: fermi_c
      !> Nuclear spin in units of \f$\frac{\hbar}{2\pi}\f$
      real(real64) :: spin
      !> Nuclear dipole moment in nuclear magnetons.
      real(real64) :: dipolemoment
      !> Nuclear quadrupole moment in barns.
      real(real64) :: quadrupolemoment
   end type isodata

   !> Reads `isodata` files.
   interface isodata_read
      module procedure isodata_read, isodata_read_unit
   end interface isodata_read

   !> Writes an `isodata` object into a file in the GRASP isotope data format.
   interface isodata_write
      module procedure isodata_write, isodata_write_unit
   end interface isodata_write

contains

   !> Reads an `isodata` file.
   !!
   !! @param filename Path to the file to be read from.
   !! @param iso Output `isodata` object.
   !! @return Returns false if there were any I/O errors.
   function isodata_read(filename, iso) result(status)
      integer, parameter :: dp = real64
      ! Arguments and return type
      character(len=*), intent(in) :: filename
      type(isodata), intent(out) :: iso
      logical :: status
      ! Local variables
      integer :: fileunit, ios
      character(255) :: iom

      open (newunit=fileunit, file=filename, status="old", iostat=ios, iomsg=iom)
      if (ios /= 0) then
         print *, "ERROR: Unable to open file:", ios, iom
         status = .false.
         return
      endif
      status = isodata_read(fileunit, iso)
      close(fileunit)
   end function isodata_read

   !> Reads an `isodata` file from a file unit.
   !!
   !! @param fileunit File unit to be read from.
   !! @param iso Output `isodata` object.
   !! @return Returns false if there were any I/O errors.
   function isodata_read_unit(fileunit, iso) result(status)
      use grasp_constants, only: fm2au => fm
      ! Arguments and return type
      integer, intent(in) :: fileunit
      type(isodata), intent(out) :: iso
      logical :: status
      ! Local variables
      integer :: ios
      character(255) :: iom

      ! Read the isodata file
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%z
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%a
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%fermi_a
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%fermi_c
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%mass
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%spin
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%dipolemoment
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom)
      if(ios /= 0) goto 999
      read (fileunit, *, iostat=ios, iomsg=iom) iso%quadrupolemoment
      if(ios /= 0) goto 999

      status = .true.
      return

      ! Error handling for IO errors (reachable via goto)
      999 continue
      print *, "ERROR: error reading from isodata file:", ios, iom
      status = .false.
      return
   end function isodata_read_unit

   !> Writes an `isodata` object into a file.
   !!
   !! @param iso The `isodata` object to be written.
   !! @param filename Path to the file to be written into.
   !! @return Returns false if there were any I/O errors.
   function isodata_write(iso, filename) result(status)
       type(isodata), intent(in) :: iso
       character(len=*), intent(in) :: filename
       logical :: status
       ! Local variables
       integer :: fileunit
       integer :: ios
       character(255) :: iom

       open(newunit=fileunit, file=filename, status='new', iostat=ios, iomsg=iom)
       if (ios /= 0) then
          print *, "ERROR: Unable to open file:", ios, iom
          status = .false.
          return
       endif
       status = isodata_write(iso, fileunit)
       close(fileunit)
   end function isodata_write

   !> Writes an `isodata` object into a file unit.
   !!
   !! @param iso The `isodata` object to be written.
   !! @param fileunit File unit to be written into.
   !! @return Returns false if there were any I/O errors.
   function isodata_write_unit(iso, fileunit) result(status)
       type(isodata), intent(in) :: iso
       integer, intent(in) :: fileunit
       logical :: status
       ! Local variables
       integer :: ios
       character(255) :: iom

       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Atomic number:'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%z
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Mass number (integer) :'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%a
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Fermi distribution parameter a:'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%fermi_a
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Fermi distribution parameter c:'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%fermi_c
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Mass of nucleus (in amu):'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%mass
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Nuclear spin (I) (in units of h / 2 pi):'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%spin
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Nuclear dipole moment (in nuclear magnetons):'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%dipolemoment
       if(ios /= 0) goto 999
       write(fileunit, '(a)', iostat=ios, iomsg=iom) 'Nuclear quadrupole moment (in barns):'
       if(ios /= 0) goto 999
       write(fileunit, *, iostat=ios, iomsg=iom) iso%quadrupolemoment
       if(ios /= 0) goto 999

       status = .true.
       return

       ! Error handling for IO errors (reachable via goto)
       999 continue
       print *, "ERROR: error writing to isodata file:", ios, iom
       status = .false.
       return
   end function isodata_write_unit

end module grasp_datafiles
