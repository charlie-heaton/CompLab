
MODULE read_files
!Edited version by charlie heaton to be able to read an alternative format of input file
! read_pdb - reads pdb files
! read_eigenfacs - reads eigenfacs format files
!
CONTAINS
! Read pdb files
  SUBROUTINE read_pdb(file,natom,atom,atnum,name,x,y,z,elem,chag)
    USE nrtype
    IMPLICIT NONE
    INTEGER, ALLOCATABLE,DIMENSION(:) :: atnum,resnum
    REAL(DP), ALLOCATABLE,DIMENSION(:) ::x,y,z,occ,bfac
    CHARACTER, ALLOCATABLE, DIMENSION(:) :: atom*6,name*4,res*3,chain*1,elem*2,chag*2
    INTEGER :: io, i, natom
    CHARACTER :: file*120, lign80*80
    
    ! Find number of atoms
    OPEN(file=pdbfile,form="FORMATTED",status="OLD",unit=2356)
    i=0
    DO
       READ(2356,'(A)',IOSTAT=io) lign80
       IF (io > 0) THEN
          WRITE(*,*) 'Check input.  Something was wrong'
          STOP
          ! End of file
       ELSE IF (io < 0) THEN
          EXIT
          ! Count number of atoms
       ELSE
          IF ((lign80(1:4).eq.'ATOM').or. &
               ((lign80(1:6).eq.'HETATM').and.hetatm)) THEN
             i=i+1
          END IF
       END IF
    END DO
    
    natom=i
    
    REWIND(2356)
    
    ALLOCATE(x(natom))
    ALLOCATE(y(natom))
    ALLOCATE(z(natom))
    ALLOCATE(elem(natom))  
    
    ! Read in atom information
    i=0
    DO
       READ(2356,'(A)',IOSTAT=io) lign80
       IF (io > 0) THEN
          WRITE(*,*) 'Check input.  Something was wrong'
          STOP
          ! End of file
       ELSE IF (io < 0) THEN
          EXIT
          ! Count number of atoms
       ELSE
             i=i+1
             READ(lign80,'(3F8.3,A2)') x(i),y(i),z(i),elem(i)
       END IF
    END DO
    
    CLOSE(2356)
    
  END SUBROUTINE read_pdb

  SUBROUTINE read_eigenfacs(filename,natom,startvec,endvec,ndim,eigenval,eigenvec,num)
   USE nrutil
   IMPLICIT NONE
   CHARACTER :: filename*120, lign80*80
   INTEGER :: natom, j, io, ndim, vector, i, k, startvec, endvec,num
   REAL(DP) :: ei
   REAL(DP),ALLOCATABLE,DIMENSION(:,:) :: eigenvec
   REAL(DP),ALLOCATABLE,DIMENSION(:) :: eigenval

 OPEN(file=filename,form="FORMATTED",status="OLD",unit=4565)

 natom=0
 j=0
 ! Count number of atoms
 DO
    READ(4565,'(A)',IOSTAT=io) lign80
    IF (io > 0) THEN
       WRITE(*,*) 'Check input.  Something was wrong'
       STOP
       ! End of file
    ELSE IF (io < 0) THEN
       EXIT
       ! Count up number of atoms
    ELSE
       IF (lign80(1:7).eq.' VECTOR') THEN
          READ(lign80,'(7X,I5)') vector
          IF ((vector.ge.startvec).and.(vector.le.endvec)) THEN
             j=j+1   
          END IF
          natom=0
       ELSE IF (lign80(1:7).eq.' ------') THEN
       ELSE
          natom = natom + 1
       END IF
    END IF
 END DO
 
 REWIND(4565)

 ALLOCATE(eigenvec(natom*ndim,j))
 ALLOCATE(eigenval(j))

 j=0
 k=0
 DO
    
    READ(4565,'(A)',IOSTAT=io) lign80
    IF (io > 0) THEN
       WRITE(*,*) 'Check input.  Something was wrong'
       STOP
       ! End of file
    ELSE IF (io < 0) THEN
       EXIT
    ELSE
       IF (lign80(1:7).eq.' VECTOR') THEN
          READ(lign80,'(7X,I5,12X,G12.4)') vector,ei
          i=0
          IF ((vector.ge.startvec).and.(vector.le.endvec)) THEN
             j=vector-startvec+1
             eigenval(j)=ei
          END IF
       ELSE IF (lign80(1:7).eq.' ------') THEN
       ELSE 
          IF ((vector.ge.startvec).and.(vector.le.endvec)) THEN
             j=vector-startvec+1
             READ(lign80,'(3(1X,G11.4))') (eigenvec(i+k,j),k=1,ndim)
             i=i+ndim
          END IF
       END IF
    END IF
    
 END DO

 num=j

 CLOSE(4565)

 END SUBROUTINE read_eigenfacs

END MODULE read_files