program createswapfile
  implicit none
  integer, parameter :: dp = selected_real_kind(15,300), k15 = selected_int_kind(15)
  integer(kind=k15), allocatable :: targets(:), swaps(:,:), uniformarrayint(:)
  real(kind=dp), allocatable :: uniformarray(:)
  integer :: i, in_unit, out_unit, istat, lines, lines2, lines3
  character(len=100) :: errormsg
  
  in_unit = 30
  lines = 0
  
  open(file="targetswaps.pdb", unit = in_unit, status = "old", action = "read", iostat = istat, &
  & iomsg = errormsg)
  if (istat /= 0) print *, errormsg
  do i = 1, 100000
    read(in_unit,*,end=800)
    lines = lines + 1
  end do
  800 continue
  lines2 = int(real(lines,dp) / 4)
  lines3 = int(real(lines,dp) / 2)
  allocate(targets(lines))
  allocate(swaps(lines2,2))
  allocate(uniformarray(lines3))
  allocate(uniformarrayint(lines3))
  
  rewind(in_unit)
  do i = 1, lines
    read(in_unit,*) targets(i)
  end do
  
  call random_number(uniformarray)
  uniformarrayint = int(uniformarray * lines3,k15)
  do i = 1, lines2
    swaps(i,1) = targets(uniformarrayint(i)+1)
  end do
  do i = 1, lines2
    swaps(i,2) = targets(uniformarrayint(i)+lines3)
  end do
  out_unit = 60
  open(file= "swappairs.txt",unit=out_unit, action = "write", status = "replace")
  do i = 1, lines2
    write(60,*) swaps(i,1), swaps(i,2)
  end do

end program createswapfile
