PROGRAM genENM
   USE nrutil
   USE utils
   USE read_files
   USE write_files
   USE foul
   IMPLICIT NONE
   LOGICAL :: getoption, hetatm, caonly, resmass, atmass, cutvect,cutassign, &
        forceres,ukn,usesec,qexist,lig1,firstat, custb, dna, hin, is_numeric
   CHARACTER :: dummy*120, inputfile*120, lign80*80, nomatm*120, res2*4, chain2*1, nomres*120, &
        foutname*120,custbfile*120
   INTEGER :: natom, io, i, j, natomold, nident, ncusres,ii,ijjjj,iseed,jat,jj, &
        ll,nntr,nnzero,nhel,nshe,k,l,fat,cbs,cbnum,nseconds,counter_1,counter_2,counter_3,counter_4
   REAL(DP) :: cutoff, cutoffdef, rave, rdev, rmin, rmax,anmp,ddf,rkh,dist,dist2,dmax,dmin, &
        distave,drms,kij,kset,shift,rx,ry,rz,trace,random,masstol,xav,yav,zav,bfacav,entot, forcing_coef,ACa,AMg,RhoCa,RhoMg
   LOGICAL,ALLOCATABLE,DIMENSION(:) :: otherres, otherchain
   INTEGER,ALLOCATABLE,DIMENSION(:) :: atnum,resnum,resnumold, resone, restwo
   REAL(DP),ALLOCATABLE,DIMENSION(:) :: x,y,z,occ,bfac,vals,mass,massold,cutvalue, &
        acutoff,kijcust
   CHARACTER,ALLOCATABLE,DIMENSION(:) :: atom*6,name*4,res*3,chain*1,elem*2,chag*2, &
        chainold*1,ident*4,cshe*1,chel*1,chainone*1,chaintwo*1,seconds*80
   CHARACTER,ALLOCATABLE,DIMENSION(:,:) :: custchain
   REAL(DP),ALLOCATABLE,DIMENSION(:,:) :: a
   INTEGER,ALLOCATABLE,DIMENSION(:,:) :: she, hel, custres
 
   IF (getoption('-help',.false.,dummy)) THEN
      CALL helptext(0)
      CALL exit(0)
   END IF
 
   iseed=27041961
   shift=1e-8
 !-----------------------------------------------------------------------------------
 ! Read in options
 
   IF (.not.getoption('-i',.true.,inputfile)) THEN
      WRITE(0,'(A)') "Need to input an input file with -i"
      CALL helptext(0)
      CALL exit(0)
   END IF
 
   INQUIRE(file=inputfile,exist=qexist)
   IF (.not.qexist) THEN
      STOP "input file does not exist"
   END IF
 
   IF (getoption('-mass',.false.,dummy)) THEN
      atmass=.true.
   ELSE
      atmass=.false.
   END IF  
 
   IF (getoption('-c',.true.,dummy)) THEN
      READ(dummy,*) cutoffdef
   ELSE
      cutoffdef=12
      WRITE(6,'(A/A)') "Cut off set to 12 A","Assign with -c if a different value is needed"
   END IF
 
   IF (getoption('-ccust',.true.,nomatm)) THEN
      cutvect=.true.
      INQUIRE(file=nomatm,exist=qexist)
      IF (.not.qexist) THEN
         STOP "ccust file specified does not exist"
      END IF 
   ELSE
      cutvect=.false.
   END IF
 
   IF (getoption('-f',.true.,dummy)) THEN
      READ(dummy,*) kset
   ELSE
      kset=1
      WRITE(6,'(A/A)') "Spring constant set to 1 kcal/mol/A^2","Assign with -f if a different value is needed"
   END IF 
 
   IF (getoption('-hine',.true.,dummy)) THEN
      IF (is_numeric(dummy(1:1))) THEN
         READ(dummy,*) rkh
      ELSE
         STOP "The value of h following -hine must be a positive real number"
      END IF
   ELSE
      rkh=-1
   END IF 
   
   IF (getoption('-hin',.false.,dummy)) THEN
      hin=.true.
   ELSE
      hin=.false.
   END IF 
 
   IF (getoption('-an',.true.,dummy)) THEN
      IF (is_numeric(dummy(1:1))) THEN
         READ(dummy,*) anmp
      ELSE
         STOP "The value of p following -an must be a positive real number"
      END IF
   ELSE
      anmp=-1
   END IF 
 
 !------------------------------------------------------------------------------------
 ! Read in pdb file
   CALL read_pdb(inputfile,natom,atnum,x,y,z,elem)
 
   WRITE(6,'(A,I5,A)') "Read in Inputfile, found", natom, " atoms"
 ! End of reading in pdb file
 !----------------------------------------------------------------------------------------- 
 
   ALLOCATE(mass(natom))
 ! Assigning atom masses
   IF (atmass) THEN
      CALL atom_mass(natom,elem,mass)
      WRITE(6,'(A)') "Assigned atoms their true mass"
   ELSE
      mass=1
      WRITE(6,'(A)') "Atom masses all set to 1 amu"
   END IF
 !-----------------------------------------------------------------------------------------
 
 ! Some stats
   WRITE(6,'(/A)')' Coordinate statistics: '
 
   CALL vecstat(x,natom,rmin,rmax,rave,rdev)
   WRITE(6,'(4(A,F12.6))')' <x>= ',rave,' +/- ',rdev,' From: ',rmin,' To: ',rmax
 
   CALL vecstat(y,natom,rmin,rmax,rave,rdev)
   WRITE(6,'(4(A,F12.6))')' <y>= ',rave,' +/- ',rdev,' From: ',rmin,' To: ',rmax
 
   CALL vecstat(z,natom,rmin,rmax,rave,rdev)
   WRITE(6,'(4(A,F12.6))')' <z>= ',rave,' +/- ',rdev,' From: ',rmin,' To: ',rmax
 
   WRITE(6,'(/A)')' Mass statistics: '
   CALL vecstat(mass,natom,rmin,rmax,rave,rdev)
   WRITE(6,'(4(A,F12.6))')' <m>= ',rave,' +/- ',rdev,' From: ',rmin,' To: ',rmax
 
 !-----------------------------------------------------------------------------------------
 
   ALLOCATE(a(3,3*natom))
 
   OPEN(file='ENM.vmd',form='FORMATTED',unit=7432)
 
   WRITE(7432,'(A)') '#!/usr/local/bin/vmd'
   WRITE(7432,'(A)') '# script for VMD (Visual Molecular Dynamics)'
   WRITE(7432,'(A)') '# Goal: visualizing the elastic network'
   WRITE(7432,'(A)') '# Type: vmd -e this-file'
   WRITE(7432,'(A)') 'color Display {Background} white'
   WRITE(7432,'(A)') 'mol new'
   WRITE(7432,'(A)') 'draw color black'
 
   OPEN(file='matrix.sdijf',form='FORMATTED',unit=9432)
 
   trace=0.d0
   dmin=0.d0
   dmax=0.d0
   distave=0.d0
   drms=0.d0
   nnzero=0
   nntr=0
   ll=0
   entot=0.d0
   !  Allocate born- mayer parameters for CaO and MgO (as calculated in Catlow and Lewis 1985). MgO value for A is for 
   !  when MgO is in an octahedral lattice. The rho values are in Angstrom.
   
   ACa   = 129228203.2
   AMg   = 75324368.0
   RhoCa = 0.3372
   RhoMg = 0.3242

      DO i=1,natom
      ii=3*i-2
 
      DO j=1,3*natom
         a(1,j)=0.d0
         a(2,j)=0.d0
         a(3,j)=0.d0
      END DO
      
      DO j=1,natom
         IF (i.ne.j) THEN
            jj=3*j-2
            !------------------------------------------------------------------------
            ! Set spring constant to custom between residues if required
            kij=kset
             
            rx=x(i)-x(j)
            ry=y(i)-y(j)
            rz=z(i)-z(j)
            dist2=rx*rx + ry*ry + rz*rz
            dist=sqrt(dist2)
            
            ! If a cutoff vector has been defined then the atom-atom cutoff 
            ! is taken here as the larger of the 2 values
            cutoff=cutoffdef
            IF (cutvect) THEN
               cutoff=max(acutoff(i),acutoff(j))
            END IF
            
            IF (hin) THEN
               IF (dist.lt.4.0) THEN
                  kij=kij*(205.54*dist-571.21)
               ELSE 
                  kij=kij*(305920/dist**6)
               END IF
            ! Adjust spring constant for Hinsen exp model
            ELSEIF (rkh.gt.0.d0) THEN
               kij=kij*exp(-(dist/rkh)**2.d0)   
            ! Adjust spring constant for anisotropic network model
            ELSEIF (anmp.gt.0.d0) THEN
               kij=kij/(dist**anmp)
            END IF
            !  Allow for different 'spring' constants for different sets of cation anion pairs. 
            !  The paramters used in the born mayer potentials were found by 
            !  Lewis and Catlow in G V Lewis and C R A Catlow 1985 J. Phys. C: Solid State Phys. 18 1149 
            !  The coefficent to the exponential has been converted from eV to Joules to fit with units used in
            !  DDPT.  
            IF (dist .le. 6) THEN

               IF ((elem(i) == 'O' .and. elem(j) == 'Mg') .or. (elem(i) == 'Mg' .and. elem(j) == 'O')) THEN
                  kij = AMg*dexp(-dist/RhoMg)
               END IF
               IF ((elem(i) == 'O' .and. elem(j) == 'Ca') .or. (elem(i) == 'O' .and. elem(j) == 'Ca')) THEN
                  kij = ACa*dexp((-dist)/RhoCa)
               END IF
            ELSE 
                  kij=kij
            END IF

            !--------------------------------------------------------------------------
            ! Calculation of the element harmonic potential
            IF (dist.le.cutoff) THEN 
               
               entot=entot+kij*dist2
 
               ll=ll+1
               IF (j.gt.i) THEN
                  WRITE(7432,'(A,3F12.4,A,3F12.4,A)') 'draw line {',x(i),y(i),z(i),'} {',x(j),y(j),z(j),'}'
               END IF
               
               IF (ll.eq.1.or.dist.lt.dmin) dmin=dist
               IF (ll.eq.1.or.dist.gt.dmax) dmax=dist
               
               distave=distave+dist
               drms=drms+dist2
               
               ! Diagonal elements of blocks i and j:
               !-----------------------------------
               ddf=kij/dist2
               a(1,ii)=a(1,ii)+rx*rx*ddf
               a(1,jj)=a(1,jj)-rx*rx*ddf
               a(2,ii+1)=a(2,ii+1)+ry*ry*ddf
               a(2,jj+1)=a(2,jj+1)-ry*ry*ddf
               a(3,ii+2)=a(3,ii+2)+rz*rz*ddf
               a(3,jj+2)=a(3,jj+2)-rz*rz*ddf
               
               ! Extra-diagonal elements of the two blocks:
               !---------------------------------------
               a(1,ii+1)=a(1,ii+1)+rx*ry*ddf
               a(2,ii)=a(2,ii)+rx*ry*ddf
               a(1,jj+1)=a(1,jj+1)-rx*ry*ddf
               a(2,jj)=a(2,jj)-rx*ry*ddf
               a(1,ii+2)=a(1,ii+2)+rx*rz*ddf
               a(3,ii)=a(3,ii)+rx*rz*ddf
               a(1,jj+2)=a(1,jj+2)-rx*rz*ddf
               a(3,jj)=a(3,jj)-rx*rz*ddf
               a(2,ii+2)=a(2,ii+2)+ry*rz*ddf
               a(3,ii+1)=a(3,ii+1)+ry*rz*ddf
               a(2,jj+2)=a(2,jj+2)-ry*rz*ddf
               a(3,jj+1)=a(3,jj+1)-ry*rz*ddf
            endif
         endif
      enddo
      
      ! Only the upper half of the matrix calculated
      
      ! Level-shift, to avoid the zero numerical values at 
      ! the time of the diagonalisation (minimization is perfect, 
      ! by definition). The chance is to raise the degeneration 
      ! of the six null eigenvalues, and to differentiate rotations 
      ! and translations.
      
      a(1,ii)  =a(1,ii)   + shift*random(iseed)
      a(2,ii+1)=a(2,ii+1) + shift*random(iseed)
      a(3,ii+2)=a(3,ii+2) + shift*random(iseed)
      
      DO j=ii,3*natom
         jat=(j-1)/3+1
         IF (a(1,j).ne.0.d0) THEN
            nnzero=nnzero+1
            WRITE(9432,'(2I10,1PG20.12)') ii,j,a(1,j)/sqrt(mass(i)*mass(jat))
         END IF
      END DO
      
      DO j=ii+1,3*natom
         jat=(j-1)/3+1
         IF (a(2,j).ne.0.d0) THEN
            nnzero=nnzero+1
            WRITE(9432,'(2I10,1PG20.12)')ii+1,j,a(2,j)/sqrt(mass(i)*mass(jat))
         END IF
      END DO
      
      DO j=ii+2,3*natom
         jat=(j-1)/3+1
         IF (a(3,j).ne.0.d0) THEN
            nnzero=nnzero+1
            WRITE(9432,'(2I10,1PG20.12)') ii+2,j,a(3,j)/sqrt(mass(i)*mass(jat))
         END IF
      END DO
      
      nntr=nntr+1
      
      trace=trace+(a(1,ii)+a(2,ii+1)+a(3,ii+2))/mass(i)
   END DO
   
   CLOSE(9432)
   
   WRITE(7432,'(2A)') 'mol load pdb ',inputfile
   CLOSE(7432)
   
   WRITE(6,'(/A,F8.4,A)')' The matrix is ', 100.d0*dfloat(nnzero)/dfloat(3*natom*(3*natom+1)/2),' % Filled.'
   WRITE(6,'(I12,A)') nnzero,'  non-zero elements.'
   distave=distave/float(ll)
   drms=drms/float(ll)-distave**2.d0
   IF (drms.gt.0.d0) drms=sqrt(drms)
   WRITE(6,'(/A,F9.2,A,F9.2/(A,F9.2))') ' Average dist.  = ',distave,' +/- ',drms, &
        ' Maximum dist.  = ',dmax,' Minimum dist.  = ',dmin
   
   WRITE(6,'(/A,1PG13.6)') ' Matrix trace   = ',trace
 
   DEALLOCATE(a)
 
   WRITE(6,'(/A)')' Hessian matrix ready.'
   
   WRITE(6,'(A,1PG13.6,A)')' Potential energy:', entot/2, 'kcal mol-1'

 END PROGRAM genENM
 
 FUNCTION RANDOM(ISEED)
 !C-----------------------------------------------------------------------
 !C     RANDOM NUMBER GENERATOR: UNIFORM DISTRIBUTION (0,1)
 !C     ISEED: SEED FOR GENERATOR. ON THE FIRST CALL THIS HAS TO
 !C     HAVE A VALUE IN THE EXCLUSIVE RANGE (1, 2147483647)
 !C     AND WILL BE REPLACED BY A NEW VALUE TO BE USED IN
 !C     FOLLOWING CALL.
 !C
 !C     REF: Lewis, P.A.W., Goodman, A.S. & Miller, J.M. (1969)
 !C     "Pseudo-random number generator for the System/360", IBM
 !C     Systems Journal 8, 136.
 !C
 !C     This is a "high-quality" machine independent generator.
 !C     INTEGERS are supposed to be 32 bits or more.
 !C     The same algorithm is used as the basic IMSL generator.
 !C
 !C     Author: Lennart Nilsson
 !C
   USE nrutil
   IMPLICIT NONE
   INTEGER :: ISEED
   REAL(DP) :: DSEED,DIVIS,DENOM,MULTIP
   REAL(DP) :: random
   DATA DIVIS/2147483647.D0/
   DATA DENOM /2147483711.D0/
   DATA MULTIP/16807.D0/
 !C
   IF(ISEED.LE.1) ISEED=314159
   DSEED=MULTIP*ISEED
   DSEED=MOD(DSEED,DIVIS)
   RANDOM=DSEED/DENOM
   ISEED=DSEED
   !C
 END FUNCTION RANDOM
 
 SUBROUTINE helptext(iunit)
   IMPLICIT NONE
   INTEGER :: iunit
   WRITE(iunit,'(A)')"                           G  e  n  E  N  M  M                           "
   WRITE(iunit,'(A)')"                           Edited for Comp Lab                           "
   WRITE(iunit,'(A)')"                                                                         "  
   WRITE(iunit,'(A)')"                               Written by:                               "
   WRITE(iunit,'(A)')"                      Tom Rodgers and David Burnell                      "
   WRITE(iunit,'(A)')"                         Edited by Charlie Heaton                        "
   WRITE(iunit,'(A)')"                                                                         "
   WRITE(iunit,'(A)')"This program produces the Hermitian matrix for a ENM based on the input  "
   WRITE(iunit,'(A)')"file inputed with -input. A matrix.sdijf file is produced which can be   "
   WRITE(iunit,'(A)')"diagonalised using DIAGSTD to calculate the eigenvalues and vectors.     "
   WRITE(iunit,'(A)')"                                                                         "
   WRITE(iunit,'(A)')"This program also outputs a map of the spring connections, ENM.vmd which "
   WRITE(iunit,'(A)')"can be viewed using VMD.                                                 "
   WRITE(iunit,'(A)')"                                                                         "
   WRITE(iunit,'(A)')"     Usage:                                                              "
   WRITE(iunit,'(A)')"             genENM -i inputfile [-c cut-off] [-f force]                 "
   WRITE(iunit,'(A)')"                    [-mass] [-hin] [-hine h] [-an p]                     " 
   WRITE(iunit,'(A)')"                                                                         "
   WRITE(iunit,'(A)')"Option    Type       Value       Description                             "
   WRITE(iunit,'(A)')"------------------------------------------------------------             "
   WRITE(iunit,'(A)')" -i       Input                  Input file name                         "
   WRITE(iunit,'(A)')"  -c      Input,Opt  12          Cut-off for spring connectivity         "
   WRITE(iunit,'(A)')"  -f      Input,Opt  1           Spring constant                         "
   WRITE(iunit,'(A)')" -mass    Opt                    Uses actual atom masses                 "
   WRITE(iunit,'(A)')" -hin     Opt                   Uses Hinsen fitted interactions         "
   WRITE(iunit,'(A)')" -hine    Input,Opt             Uses Hinsen exponential interactions    "
   WRITE(iunit,'(A)')"                                 instead, h is the decay factor          "
   WRITE(iunit,'(A)')"  -an     Input,Opt             Uses anisotropic interactions instead,  "
   WRITE(iunit,'(A)')"                                 p is the order of the power decay       "
  
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
 
 FUNCTION is_numeric(string)
 IMPLICIT NONE
 CHARACTER(len=*), INTENT(IN) :: string
 LOGICAL :: is_numeric
 REAL :: x
 INTEGER :: e,n
 CHARACTER(len=12) :: fmt
 
 n=LEN_TRIM(string)
 WRITE(fmt,'("(F",I0,".0)")') n
 READ(string,fmt,IOSTAT=e) x
 is_numeric = e == 0
 END FUNCTION is_numeric
 
 
