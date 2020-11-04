function getEnergy(unit, check)
integer, intent(in) :: unit
logical, intent(in) :: check
integer, parameter :: dp = selected_real_kind(15,300)
real(kind=dp) :: getEnergy
integer :: i, ipos = 1, k
character(len=51) :: temporary
logical ::  found

do i= 1, 200
 read(unit, '(A)' ) temporary
end do

do k=1, 1500
 read(unit, '(A)' ) temporary
 ipos = scan(temporary, "F")
 if (ipos.eq.3) then
  ipos = scan(temporary, "i")
  if (ipos.eq.4) then
   ipos = scan(temporary, "n")
   if (ipos.eq.5) then
    ipos = scan(temporary, "e")
    if (ipos.eq.9) then
     ipos = scan(temporary, "y")
     if (ipos.eq.14) then
      read(temporary(20:32), *) getEnergy
      exit
     end if
    else if (ipos.eq.11) then
     ipos = scan(temporary, "y")
     if (ipos.eq.19) then
      read(temporary(25:37), *) getEnergy
      found = .true.
      exit
     end if
    end if
   end if
  end if
 end if
end do
end function


program readEnergies
implicit none
integer :: istat, j, istat2, istat3, ipos
integer, parameter :: dp = selected_real_kind(15,300)
real(kind=dp) :: MgO_0Ca, MgO_1Ca, MgO_2Ca, MgO_3Ca, MgO_4Ca
real(kind=dp), external :: getEnergy
character (len=100) :: firstline
logical :: free=.false.

open(30, file = 'MgO.gin', status = 'old', iostat = istat)
if (istat.ne.0) then
 print *, 'Error opening output file'
end if
read(30, '(A)' ) firstline
close(30)
do j = 1, 89
 ipos = scan(firstline(j:89), "f")
 if (ipos.eq.1) then
  ipos = scan(firstline(j:89), "r")
  if (ipos.eq.2) then
   ipos = scan(firstline(j:89), "e")
   if (ipos.eq.3) then
    ipos = scan(firstline(j:89), "y")
    if (ipos.eq.11) then
     free = .true.
     print *, 'Free energy keyword detected'
     print *, 'Calculating using free energy'
    end if
   end if
  end if
 end if
end do

open(10,file = 'MgO_0Ca.gout', status = 'old', iostat = istat )
open(11,file = 'MgO_1Ca.gout', status = 'old', iostat = istat )
open(12,file = 'MgO_2Ca.gout', status = 'old', iostat = istat )
open(13,file = 'MgO_3Ca.gout', status = 'old', iostat = istat )
open(14,file = 'MgO_4Ca.gout', status = 'old', iostat = istat )
if (istat.ne.0) then
 print *, 'Error opening output file'
end if
MgO_0Ca = getEnergy(10, free)
MgO_1Ca = getEnergy(11, free)
MgO_2Ca = getEnergy(12, free)
MgO_3Ca = getEnergy(13, free)
MgO_4Ca = getEnergy(14, free)
close(10)
close(11)
close(12)
close(13)
close(14)

print *, MgO_0Ca, MgO_1Ca, MgO_2Ca, MgO_3Ca, MgO_4Ca

open(unit=15, file = 'energies.dat', status = 'replace', iostat = istat2)
if (istat2.ne.0) then
 print *, 'Error creating file'
end if
write(15, *, iostat = istat3) 0, MgO_0Ca
write(15, *, iostat = istat3) 1, MgO_1Ca
write(15, *, iostat = istat3) 2, MgO_2Ca
write(15, *, iostat = istat3) 3, MgO_3Ca
write(15, *, iostat = istat3) 4, MgO_4Ca
close(15)
if (istat3.ne.0) then
 print *, 'Error writing to file'
end if

call execute_command_line('gnuplot -p plot.plt')

end program
