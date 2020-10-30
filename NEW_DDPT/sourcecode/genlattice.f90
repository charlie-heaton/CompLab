!Creates lattice coordinate input file for GENENMM
program GenLattice
    implicit none
    character(len=100) :: errormsg,dummy,swapfile
    character(len=2) :: atom1, atom2
    
    integer, parameter :: dp = selected_real_kind(15,300), n = 4
    integer :: xcounter = 0, ycounter = 0, zcounter = 0, i, j, out_unit, istat
    
    real(kind=dp) :: a1,a2 !Lattice constant, a1 for MgO, a2 for CaO
    real(kind=dp), dimension(16*(n**3), 5) :: coordinates ! (Index, xcoordinate, ycoordinate, zcoordinate, atom type)
    real(kind=dp), dimension(3) :: current_coordinates                                                    ! (Atom types: 1 = Mg, 2 = Ca, 3 = O)

    !Input variables through flags
    IF (getoption('-type1',.true.,dummy)) THEN     !These if constructs check for flag inputs, and store 
        read(dummy,*) atom1                        !them in variables. The default use of this program
    ELSE                                           !is for Mg and Ca, but inputting other atom names will
        atom1 = "Mg"                               !also work.
    END IF
    
    IF (getoption('-type2',.true.,dummy)) THEN
        read(dummy,*) atom2
    ELSE
        atom2 = "Ca"
    END IF
    
    IF (getoption('-a1',.true.,dummy)) THEN
        read(dummy,*) a1
    ELSE
        a1 = 4.38_dp
    END IF
    
    IF (getoption('-a2',.true.,dummy)) THEN
        read(dummy,*) a2
    ELSE
        a2 = 4.76_dp
    END IF
    
    !The following section of code builds the Mg (or atom1) section of crystal

    i = 1
    current_coordinates = 0.0_dp
    do while (zcounter < n)
      current_coordinates(2) = 0
      do while (ycounter < n)
      current_coordinates(1) = 0
        do while (xcounter < n)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2), current_coordinates(3), 1)                    !Using pictures of the MgO cell structure i have set up this
          call savecoordinates(i, current_coordinates(1) + (a1/2.0_dp), current_coordinates(2), current_coordinates(3), 3)      !There are 8 atoms per lattice point, 4 Mg and 4 O
          call savecoordinates(i, current_coordinates(1), current_coordinates(2) + (a1/2.0_dp), current_coordinates(3), 3)
          call savecoordinates(i, current_coordinates(1) + (a1/2.0_dp), &
          & current_coordinates(2) + (a1/2.0_dp), current_coordinates(3), 1)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2),&
          & current_coordinates(3) + (a1/2.0_dp), 3)
          call savecoordinates(i, current_coordinates(1) + (a1/2.0_dp),& 
          & current_coordinates(2), current_coordinates(3) + (a1/2.0_dp), 1)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2)&
          & + (a1/2.0_dp), current_coordinates(3) + (a1/2.0_dp), 1)
          call savecoordinates(i, current_coordinates(1) + (a1/2.0_dp), &
          & current_coordinates(2) + (a1/2.0_dp), current_coordinates(3) + (a1/2.0_dp), 3)
          current_coordinates(1) = current_coordinates(1) + (a1)
          xcounter = xcounter + 1
        end do !When 10 lattice points have been generated in the x direction, it loops back to the start and increments in the y direction
        current_coordinates(2) = current_coordinates(2) + (a1) 
        ycounter = ycounter + 1
        xcounter = 0
      end do
      current_coordinates(3) = current_coordinates(3) + (a1)
      zcounter = zcounter + 1
      ycounter = 0
    end do
    
    current_coordinates(1) = -1 * (a2) !Once the MgO crystal has been created, we move near to where we started but displaced by one lattice constant in the negative x direction
    current_coordinates(2) = 0         !The CaO crystal is built in the same way as the MgO crystal, but in the negative x direction as it stretches away from the boundary
    current_coordinates(3) = 0
    xcounter = 0
    ycounter = 0
    zcounter = 0
    
    do while (zcounter < n)
      current_coordinates(2) = 0
      do while (ycounter < n)
      current_coordinates(1) = -a2
        do while (xcounter < n)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2), current_coordinates(3), 2)      
          call savecoordinates(i, current_coordinates(1) - (a2/2.0_dp), current_coordinates(2), current_coordinates(3), 3)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2) + (a1/2.0_dp), current_coordinates(3), 3)
          call savecoordinates(i, current_coordinates(1) - (a2/2.0_dp), &
          & current_coordinates(2) + (a2/2.0_dp), current_coordinates(3), 2)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2),&
          & current_coordinates(3) + (a2/2.0_dp), 3)
          call savecoordinates(i, current_coordinates(1) - (a2/2.0_dp),& 
          & current_coordinates(2), current_coordinates(3) + (a2/2.0_dp), 2)
          call savecoordinates(i, current_coordinates(1), current_coordinates(2)&
          & + (a2/2.0_dp), current_coordinates(3) + (a2/2.0_dp), 2)
          call savecoordinates(i, current_coordinates(1) - (a2/2.0_dp), &
          & current_coordinates(2) + (a2/2.0_dp), current_coordinates(3) + (a2/2.0_dp), 3)
          current_coordinates(1) = current_coordinates(1) - (a2)
          xcounter = xcounter + 1
        end do
        current_coordinates(2) = current_coordinates(2) + (a2) 
        ycounter = ycounter + 1
        xcounter = 0
      end do
      current_coordinates(3) = current_coordinates(3) + (a2)
      zcounter = zcounter + 1
      ycounter = 0
    end do
    
  IF (getoption('-swaps',.true.,dummy)) THEN    !If we have inputted a swap file, then the swap function
    read(dummy,*) swapfile                      !is now run.
    call swaps(swapfile)
  END IF
  
  
  

    
    
  
  !The following code writes out the lattice to a DDPT compatible input file.
  !The if statement is used to convert integer atom type into the atom abbreviation
        
  out_unit = 30
  open(file="input.pdb", unit = out_unit, status = "replace", iostat = istat, iomsg = errormsg)
  if (istat /= 0) print *, errormsg      
  do j = 1, i-1
    if (int(coordinates(j,5)) == 1) then                                                  !This if statement replaces the real number representing the atom type with the text abbreviation
      write(out_unit,fmt='(I5,3F8.3,A4)') int(coordinates(j,1)), coordinates(j,2), coordinates(j,3), coordinates(j,4), atom1
    else if (int(coordinates(j,5)) == 2) then
      write(out_unit,fmt='(I5,3F8.3,A4)') int(coordinates(j,1)), coordinates(j,2), coordinates(j,3), coordinates(j,4), atom2
    else if (int(coordinates(j,5)) == 3) then
      write(out_unit,fmt='(I5,3F8.3,A4)') int(coordinates(j,1)), coordinates(j,2), coordinates(j,3), coordinates(j,4), " O"
    else
      print *, "Error"                                              
    end if
  end do
  
   
  contains  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  
  subroutine savecoordinates(i, x, y, z, atype)
    real(kind=dp) :: x, y, z
    integer :: atype, i
    coordinates(i,1) = real(i,dp)
    coordinates(i,2) = x
    coordinates(i,3) = y
    coordinates(i,4) = z
    coordinates(i,5) = real(atype)
    i = i + 1
  end subroutine savecoordinates
  
  
  subroutine swaps(swapfile)
    character(len=100) :: swapfile
    integer :: index1, index2, out_unit, i
    real(kind=dp), dimension(3) :: temp
    out_unit = 40
    open(file=swapfile, unit=out_unit, status = "old", iostat = istat, iomsg = errormsg)
    do i = 1, 10000000
      read(out_unit,*, end=800) index1, index2 !The end label specifies where to jump to when file ends
      if ((coordinates(index1,5) == 3) .or. (coordinates(index2,5) == 3)) then
        cycle      !We want to avoid swapping any oxygen atoms around
      end if
      temp(1) = coordinates(index1,2)  !Store the first atoms coordinates
      temp(2) = coordinates(index1,3)
      temp(3) = coordinates(index1,4)
      coordinates(index1,2) = coordinates(index2,2) !Change the first atoms coordinates to the second
      coordinates(index1,3) = coordinates(index2,3)
      coordinates(index1,4) = coordinates(index2,4)
      coordinates(index2,2) = temp(1) !Move the second atom to the first set of coordinates
      coordinates(index2,3) = temp(2)
      coordinates(index2,4) = temp(3)
    end do  
  800 continue  
  end subroutine
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  !Edit made by Charlie Heaton to allow for flag inputs
  FUNCTION getoption(flag,getval,cvalue)
    IMPLICIT NONE
    CHARACTER(*):: flag,cvalue
    CHARACTER(160):: arg
    LOGICAL ::getoption,getval
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
    IF (getval) CALL getarg(i+1,cvalue)
  END FUNCTION getoption

  end program GenLattice
