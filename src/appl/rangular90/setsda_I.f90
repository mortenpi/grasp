      MODULE setsda_I
      INTERFACE
!...Generated by Pacific-Sierra Research 77to90  4.3E  14:52:18   1/ 6/07
!...Modified by Charlotte Froese Fischer
!                     Gediminas Gaigalas  10/05/17
      SUBROUTINE setsda (OUTSDA, NNONZ, LPRINT, NB, MYID, NPROCS, FHEAD)
      EXTERNAL OUTSDA
      INTEGER, INTENT(IN) :: NNONZ
      LOGICAL :: LPRINT
      INTEGER, INTENT(IN) :: NB
      INTEGER, INTENT(IN) :: MYID
      INTEGER, INTENT(IN) :: NPROCS
      CHARACTER (LEN = *), INTENT(IN) :: FHEAD
      END SUBROUTINE
      END INTERFACE
      END MODULE
