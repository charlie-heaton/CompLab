PROGRAM write_coordinates
IMPLICIT NONE
CHARACTER           :: dummy*120
LOGICAL             :: getoption
INTEGER, PARAMETER  :: dp = selected_real_kind(15, 307)
REAL(KIND=dp)       :: x,y,z,zero
INTEGER             :: io
zero = 0.0

IF(getoption('-x',.true.,dummy)) THEN
     READ(dummy,*) x
ELSE
    STOP "Input a value for the lattice constant"
END IF

IF(getoption('-y',.true.,dummy)) THEN
     READ(dummy,*) y
ELSE
    STOP "Input a value for the lattice constant"
END IF

IF(getoption('-z',.true.,dummy)) THEN
     READ(dummy,*) z
ELSE
    STOP "Input a value for the lattice constant"
END IF

OPEN(UNIT=1000,FILE="coordinates.dat",status="unknown",IOSTAT=io)
IF(io .ne. 0) STOP "Error Opening File"

WRITE(UNIT=1000,IOSTAT=io,fmt='(A19)') "%BLOCK LATTICE_CART"
IF(io .ne. 0) STOP "Error Writing File 1 "
WRITE(UNIT=1000,fmt='(4X,F12.10,4X,F12.10,4X,F12.10)',IOSTAT=io) x,zero,zero
IF(io .ne. 0) STOP "Error Writing File 2 "
WRITE(UNIT=1000,fmt='(4X,F12.10,4X,F12.10,4X,F12.10)',IOSTAT=io) zero,y,zero
IF(io .ne. 0) STOP "Error Writing File 2 "
WRITE(UNIT=1000,fmt='(4X,F12.10,4X,F12.10,4X,F12.10)',IOSTAT=io) zero,zero,z
IF(io .ne. 0) STOP "Error Writing File 2 "
WRITE(UNIT=1000,IOSTAT=io,fmt='(A22)') "%ENDBLOCK LATTICE_CART"
IF(io .ne. 0) STOP "Error Writing File 3 "

CLOSE(1000)

END PROGRAM

FUNCTION getoption(flag,getval,cvalue)
  IMPLICIT NONE
  CHARACTER(*) :: flag,cvalue
  CHARACTER(160) :: arg
  LOGICAL :: getoption,getval
  INTEGER :: l,i,j
  
  getoption=.false.
  i=0
  DO j=1,iargc()
     CALL getarg(j,arg)
     IF (arg.eq.flag) i=j
  END DO
  IF (i.gt.0) THEN
     getoption=.true.
  END IF
  IF (getval) call getarg(i+1,cvalue)
END FUNCTION getoption
