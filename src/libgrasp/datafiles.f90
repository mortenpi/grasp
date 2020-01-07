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

   !> Represents a single orbital, as stored in an `rwfn` / `.w` file.
   !!
   !! The orbital, labelled by the \f$n\f$ and \f$\kappa\f$ quantum numbers, is
   !! assumed to have the following form
   !!
   !! \f[
   !!   \phi_{m}(n\kappa; r, \theta, \phi) = \frac{1}{r}
   !!   \left( \begin{array}{c}
   !!     P(r) \chi_{\kappa m}(\theta, \phi) \\
   !!     i Q(r) \chi_{-\kappa m}(\theta, \phi)
   !!   \end{array} \right)
   !! \f]
   !!
   !! where \f$\chi_{\kappa m}(\theta, \phi)\f$ are the analytically given
   !! spin-spherical harmonics. The radial functions \f$P(r)\f$ and \f$Q(r)\f$
   !! are defined on a grid.
   type orbital
      !> Principal quantum number \f$n\f$.
      integer(int32) :: n
      !> Angular momentum quantum number \f$\kappa\f$.
      integer(int32) :: kappa
      real(real64) :: e, pz
      !> Number of points in the `r`, `p` and `q` arrays.
      integer(int32) :: npts
      !> Contains the radial grid.
      real(real64), allocatable, dimension(:) :: r
      !> Array containing the values of the large component \f$P(r)\f$.
      real(real64), allocatable, dimension(:) :: p
      !> Array containing the values of the small component \f$Q(r)\f$.
      real(real64), allocatable, dimension(:) :: q
    end type

    !> Read an orbital (`.w`) file into a list of `orbital` objects.
   interface orbitals_read
      module procedure orbitals_read, orbitals_read_unit
   end interface orbitals_read

   !> Writes a list of `orbital` objects into an orbital (`.w`) file.
  interface orbitals_write
     module procedure orbitals_write, orbitals_write_unit
  end interface orbitals_write

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

   function orbitals_read(filename, orbitals) result(status)
      ! Arguments and return type
      character(len=*), intent(in) :: filename
      type(orbital), allocatable, dimension(:), intent(out) :: orbitals
      logical :: status
      ! Local variables
      integer :: fileunit, ios
      character(255) :: iom

      open (newunit=fileunit, file=filename, status="old", form="unformatted", iostat=ios, iomsg=iom)
      if (ios /= 0) then
         print *, "ERROR: Unable to open file:", ios, iom
         status = .false.
         return
      endif
      status = orbitals_read(fileunit, orbitals)
      close(fileunit)
   end function orbitals_read

   function orbitals_read_unit(fileunit, orbitals) result(status)
      ! Arguments and return type
      integer, intent(in) :: fileunit
      type(orbital), allocatable, dimension(:), intent(out) :: orbitals
      logical :: status
      ! Local variables
      integer :: ios, i, norbitals
      character(255) :: iom
      character(6) :: g92mix
      type(orbital), allocatable, dimension(:) :: orbitals_tmp
      logical :: ioerror

      g92mix = '000000' ! initialized to something
      read (fileunit, iostat=ios, iomsg=iom) g92mix
      if(ios /= 0) goto 999
      if (g92mix /= 'G92RWF') then
         print '(a," (",a6,")")', "ERROR: Invalid file header", g92mix
         status = .false.
         return
      endif

      norbitals = 0
      allocate(orbitals_tmp(64))
      do while(readnextorbital(ioerror))
         if(ioerror) goto 999
      enddo

      allocate(orbitals(norbitals))
      orbitals(:) = orbitals_tmp(1:norbitals)
      status = .true.
      return

      ! Error handling for IO errors (reachable via goto)
      999 continue
      print *, "ERROR: error reading from isodata file:", ios, iom
      status = .false.
      return

   contains

      function readnextorbital(ioerror)
         logical, intent(out) :: ioerror
         logical :: readnextorbital

         integer :: npy, naky, my
         real(real64) :: ey
         real(real64), allocatable, dimension(:) :: py, qy, ry
         type(orbital), allocatable, dimension(:) :: orbitals_swap

         ! Try reading the orbital information. If this fails with some IO error,
         ! then we assume that we have run out of orbitals in the file.
         read(fileunit, iostat=ios, iomsg=iom) npy, naky, ey, my
         if(ios /= 0) then
            readnextorbital = .false.
            return
         endif

         ! If we've run out of space in orbitals_tmp, then we double its allocation
         if(norbitals >= size(orbitals_tmp)) then
            orbitals_swap = orbitals_tmp
            deallocate(orbitals_tmp)
            allocate(orbitals_tmp(2*size(orbitals_swap)))
            orbitals_tmp(1:size(orbitals_swap)) = orbitals_swap(:)
            deallocate(orbitals_swap)
         endif

         ! Add the current orbital to the orbitals_tmp array
         norbitals = norbitals + 1
         orbitals_tmp(norbitals)%n = npy
         orbitals_tmp(norbitals)%kappa = naky
         orbitals_tmp(norbitals)%e = ey
         orbitals_tmp(norbitals)%npts = my
         ! Allocate and read in the P, Q and R arrays too
         allocate(orbitals_tmp(norbitals)%p(my))
         allocate(orbitals_tmp(norbitals)%q(my))
         allocate(orbitals_tmp(norbitals)%r(my))
         read(fileunit, iostat=ios, iomsg=iom)      &
            orbitals_tmp(norbitals)%pz,             &
            (orbitals_tmp(norbitals)%p(i), i=1,my), &
            (orbitals_tmp(norbitals)%q(i), i=1,my)
         if(ios /= 0) goto 999
         read(fileunit, iostat=ios, iomsg=iom) (orbitals_tmp(norbitals)%r(i), i=1,my)
         if(ios /= 0) goto 999

         readnextorbital = .true. ! There might be another one coming..
         ioerror = .false.
         return

         ! Error handling for IO errors (reachable via goto)
         999 continue
         print *, "ERROR: error reading from isodata file:", ios, iom
         ioerror = .true.
         return
      end function readnextorbital

   end function orbitals_read_unit

   !> Writes list of `orbital` objects into a file in the GRASP `.w` format.
   !!
   !! @param orbitals A list of `orbital` objects.
   !! @param filename Path to the file to be written into.
   !! @return Returns false if there were any I/O errors.
   function orbitals_write(orbitals, filename) result(status)
       type(orbital), dimension(:), intent(in) :: orbitals
       character(len=*), intent(in) :: filename
       logical :: status
       ! Local variables
       integer :: fileunit
       integer :: ios
       character(255) :: iom

       open(newunit=fileunit, file=filename, status='new', form='unformatted', iostat=ios, iomsg=iom)
       if (ios /= 0) then
          print *, "ERROR: Unable to open file for writing:", ios, iom
          status = .false.
          return
       endif
       status = orbitals_write(orbitals, fileunit)
       close(fileunit)
   end function orbitals_write

   !> Writes list of `orbital` objects into a file opened on `fileunit` in the
   !! GRASP `.w` format.
   !!
   !! @param orbitals A list of `orbital` objects.
   !! @param fileunit File unit to be written into.
   !! @return Returns false if there were any I/O errors.
   function orbitals_write_unit(orbitals, fileunit) result(status)
       type(orbital), dimension(:), intent(in) :: orbitals
       integer, intent(in) :: fileunit
       logical :: status
       ! Local variables
       integer :: ios, i, k, npts
       character(255) :: iom

       ! Write the file header
       write(fileunit, iostat=ios, iomsg=iom) 'G92RWF'
       if(ios /= 0) goto 999

       do k = 1, size(orbitals)
          npts = orbitals(k)%npts
          write(fileunit, iostat=ios, iomsg=iom) orbitals(k)%n, orbitals(k)%kappa, orbitals(k)%e, npts
          if(ios /= 0) goto 999
          write(fileunit, iostat=ios, iomsg=iom) orbitals(k)%pz, (orbitals(k)%p(i), i=1,npts), (orbitals(k)%q(i), i=1,npts)
          if(ios /= 0) goto 999
          write(fileunit, iostat=ios, iomsg=iom) (orbitals(k)%r(i), i=1,npts)
          if(ios /= 0) goto 999
       enddo

       status = .true.
       return

       ! Error handling for IO errors (reachable via goto)
       999 continue
       print *, "ERROR: error writing to orbital file:", ios, iom
       status = .false.
       return
   end function orbitals_write_unit

end module grasp_datafiles
