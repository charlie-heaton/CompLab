PROGRAM spacing
  USE nrtype
  USE read_files
  IMPLICIT NONE
  INTEGER, ALLOCATABLE,DIMENSION(:) :: atnum,resnum,resnumold
  REAL(DP), ALLOCATABLE,DIMENSION(:) ::x,y,z,occ,bfac
  REAL(DP) :: masstol, xav, yav, zav, bfacav
  CHARACTER, ALLOCATABLE, DIMENSION(:) :: atom*6,name*4,res*3,chain*1,chainold*1,elem*2,chag*2
  INTEGER :: io, i, natom, natomold, j, fat
  CHARACTER :: inputfile*120, lign80*80, dummy*120
  LOGICAL :: hetatm,getoption,caonly,lig1,qexist,firstat
  
  IF (getoption('-help',.false.,dummy)) THEN
     CALL helptext(0)
     CALL exit(0)
  END IF
  
  IF (.not.getoption('-i',.true.,inputfile)) THEN
     WRITE(0,'(A)') "Need to input a file with -i"
     CALL helptext(0)
     CALL exit(0)
  END IF
  
  INQUIRE(file=inputfile,exist=qexist)
  IF (.not.qexist) THEN
     STOP "input file does not exist"
  END IF
  CALL read_pdb(inputfile,natom,atnum,x,y,z,elem)
  
  OPEN(file='dist.dat',form='FORMATTED',unit=6434)
  DO i=1,natom
     DO j=1,natom
        WRITE(6434,'(1X,I5,1X,I5,1X,G11.4)') atnum(i), atnum(j), ((x(i)-x(j))**2+(y(i)-y(j))**2+(z(i)-z(j))**2)**0.5
     END DO
     WRITE(6434,'(1X)')
  END DO
  CLOSE(6434)



END PROGRAM spacing


SUBROUTINE helptext(iunit)
  IMPLICIT NONE
  INTEGER :: iunit
  WRITE(iunit,'(A)')"                           S  P  A  C  I  N  G                           "
  WRITE(iunit,'(A)')"                               VERSION 1.0                               "
  WRITE(iunit,'(A)')"                                                                         "  
  WRITE(iunit,'(A)')"                               Written by:                               "
  WRITE(iunit,'(A)')"                      Tom Rodgers and David Burnell                      "
  WRITE(iunit,'(A)')"                                                                         "
  WRITE(iunit,'(A)')"This program produces a file, dist.dat for plotting the spacing between  "
  WRITE(iunit,'(A)')"atoms.                                                                   "
  WRITE(iunit,'(A)')"                                                                         "
  WRITE(iunit,'(A)')"     Usage:                                                              "
  WRITE(iunit,'(A)')"             spacing -pdb pdbfile [-ca] [-het] [-lig1]                   "
  WRITE(iunit,'(A)')"                                                                         "
  WRITE(iunit,'(A)')"Option    Type       Value       Description                             "
  WRITE(iunit,'(A)')"------------------------------------------------------------             "
  WRITE(iunit,'(A)')" -pdb     Input                  pdb file name                           "
  WRITE(iunit,'(A)')" -ca       Opt                   Calculates Ca model only                "
  WRITE(iunit,'(A)')" -lig1     Opt                   Assigns each residue in the HETATMs one "
  WRITE(iunit,'(A)')"                                 average point, only works with -het and "
  WRITE(iunit,'(A)')"                                 -ca flags as well                       "
  WRITE(iunit,'(A)')" -het      Opt                   Includes reading HETATM records         "
  WRITE(iunit,'(A)')"                                                                         "
 
END SUBROUTINE helptext

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
