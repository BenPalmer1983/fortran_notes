! computer.nml
!
! &COMPUTER
! os='Ubuntu'
! ram=64
! proc%make='AMD'
! proc%model='Ryzen 9'
! proc%freq=3.8
! proc%cores=12
! proc%cache(1)=0.75
! proc%cache(2)=6
! proc%cache(3)=64
! drive(1)=256
! drive(2)=5000
! drive(3)=5000
! drive(4)=0
! drive(5)=0
! /

SUBROUTINE readnamelist(filepath)
!########################################################################
IMPLICIT NONE
!########################################################################
CHARACTER(LEN=*), INTENT(IN) ::                      filepath
!########################################################################
TYPE :: t_processor
  CHARACTER(len=16) ::                               make
  CHARACTER(len=16) ::                               model
  INTEGER(kind=INT32) ::                             cores 
  REAL(kind=REAL32) ::                               freq 
  REAL(kind=REAL32) ::                               cache(1:3) 
END TYPE t_processor
CHARACTER(len=16) ::                                 os
INTEGER(kind=INT32) ::                               ram
TYPE(t_processor) ::                                 processor
INTEGER(kind=INT32) ::                               drive(1:5)
LOGICAL ::                                           exists
INTEGER(kind=INT32) ::                               status
INTEGER(kind=INT32) ::                               fh
!########################################################################
NAMELIST /COMPUTER/ os, ram, processor, drive     ! Define namelist
!########################################################################
! Check file exists
INQUIRE (file=filepath, exist=exists)
IF(.NOT. exists)THEN
  STOP "File " // TRIM(filepath) // " does not exist"
END IF
! Open and read file 
OPEN (action='read', file=filepath, iostat=status, newunit=fh)
read (nml=COMPUTER, iostat=status, unit=fh)
CLOSE(fh)
! Check read successful
IF(status .NE. 0)THEN
  STOP "File " // TRIM(filepath) // " contains an incorrect namelist format"
END IF
! Output
WRITE (*, "(A20)") ADJUSTR(os)
WRITE (*, "(I20)") ram
WRITE (*, "(A20)") ADJUSTR(processor%make)
WRITE (*, "(A20)") ADJUSTR(processor%model)
WRITE (*, "(I20)") processor%cores
WRITE (*, "(I20)") drive(1)
WRITE (*, "(I20)") drive(2)
WRITE (*, "(I20)") drive(3)
WRITE (*, "(I20)") drive(4)
WRITE (*, "(I20)") drive(5)
END SUBROUTINE readnamelist