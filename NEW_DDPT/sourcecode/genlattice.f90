program GenLattice
    implicit none
    character(len=100) :: errormsg
    
    integer, parameter :: dp = selected_real_kind(15,300), n = 4
    integer :: xcounter = 0, ycounter = 0, zcounter = 0, i, j, out_unit, istat
    
    real(kind=dp), parameter :: a1 = 4.212, a2 = 2.40 !Lattice constant, a1 for MgO, a2 for CaO
    real(kind=dp), dimension(16*(n**3), 5) :: coordinates ! (Index, xcoordinate, ycoordinate, zcoordinate, atom type)
    real(kind=dp), dimension(3) :: current_coordinates                                                    ! (Atom types: 1 = Mg, 2 = Ca, 3 = O)
    
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
    
    
    
    
  
    
        
  out_unit = 30
  open(file="input.pdb", unit = out_unit, status = "replace", iostat = istat, iomsg = errormsg)
  if (istat /= 0) print *, errormsg    
  
  do j = 1, i-1
    if (int(coordinates(j,5)) == 1) then                                                  !This if statement replaces the real number representing the atom type with the text abbreviation
      write(out_unit,*) int(coordinates(j,1)), coordinates(j,2), coordinates(j,3), coordinates(j,4), " Mg"    
    else if (int(coordinates(j,5)) == 2) then
      write(out_unit,*) int(coordinates(j,1)), coordinates(j,2), coordinates(j,3), coordinates(j,4), " Ca" 
    else if (int(coordinates(j,5)) == 3) then
      write(out_unit,*) int(coordinates(j,1)), coordinates(j,2), coordinates(j,3), coordinates(j,4), " O"
    else
      print *, "Error"                                              !If something has gone wrong and the atom is none of the 3 types this is flagged here
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
  
  
  
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  end program GenLattice