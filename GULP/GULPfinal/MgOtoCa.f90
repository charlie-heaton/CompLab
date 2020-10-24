program MgOtoCa
implicit none

integer :: i    !number of Ca atoms
integer :: k = 0    !couter for amount of Mg atoms read
integer :: ios
integer, parameter :: dp = selected_real_kind(15,300)

integer :: j    !Integer to count lines read
integer, parameter :: &
     n=14, &    !line to skip first
     p=4,   &   !lines to read
     q=1,    &  !lines to skip over blank space
     r= 34      !rest of the lines needed to read into file

!defines elements read from MgO; Mg atoms, Ca atoms and the entire line
character (len=2) :: Mg = 'Mg' 
character (len=2) :: Ca = 'Ca'
character (len=140) :: line

read*, i    !reads in i from MgtoCa.sh

open(12,file='MgO.gin',status='old',iostat=ios) !opens MgO.gin file
if (ios /= 0) stop "Error opening file MgO.gin"
   
open(unit=13, file = 'MgOtoCa.dat', status = 'new') !creates new file to rewrite MgO.gin into with desired atom number

!Skips firstlines of file by just reading them and writing to output
do j=1,n 
    read(12,'(A)') line
    write(13,'(A)') line
end do

!Now at core Mg atoms, so reads them and changes desired amount of Mg to Ca 
do j=1,p 
    read(12,'(A)') line 
   if (line(5:6) == Mg) then    !Atom type is always in columns 5 and 6 of MgO.gin
        k = k + 1               !Counts amount of Mg atoms read
        if ((4-i)<=k) then      !Only 4 core atoms so want '4-i' Mg atoms total
            line(5:6) = 'Ca'    
        end if
    end if
    write(13,'(A)') line
end do

!Skips blank line between core and shell atoms
do j=1,q
read(12,'(A)') line
write(13,'(A)') line
end do

k = 0 !Resets Mg atoms counted

!Now does same thing as core to shell atoms; want to replace same amount of core and shell, so code the same
do j=1,p 
    read(12,'(A)') line 
   if (line(5:6) == Mg) then
        k = k + 1
        if ((4-i)<=k) then
            line(5:6) = 'Ca'
        end if
    end if
    write(13,'(A)') line
end do

!copies rest of MgO.gin to new file
do j=1,r
read(12,'(A)') line
write(13,'(A)') line
end do

!closes both files
close(unit=12)
close(unit=13)

end program MgOtoCa
