function getEnergy(unit) result(energy)
integer, intent(in) :: unit
integer :: i, ipos = 1
character(len=51) :: temporary
integer, parameter :: dp = selected_real_kind(15,300)
real(kind=dp) :: energy
logical :: found = .false.

do i= 1, 200
 read(unit, '(A)' ) temporary
end do

do while (found.eqv..false.)
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
      read(temporary(20:32), *) energy
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
integer :: istat, j, istat2, istat3
integer, parameter :: dp = selected_real_kind(15,300)
real(kind=dp) :: MgO_0Ca, MgO_1Ca, MgO_2Ca, MgO_3Ca, MgO_4Ca, getEnergy

open(10,file = 'MgO_0Ca.gout', status = 'old', iostat = istat )
open(11,file = 'MgO_1Ca.gout', status = 'old', iostat = istat )
open(12,file = 'MgO_2Ca.gout', status = 'old', iostat = istat )
open(13,file = 'MgO_3Ca.gout', status = 'old', iostat = istat )
open(14,file = 'MgO_4Ca.gout', status = 'old', iostat = istat )
if (istat.ne.0) then
 print *, 'Error opening output file'
end if

MgO_0Ca = getEnergy(10)
MgO_1Ca = getEnergy(11)
MgO_2Ca = getEnergy(12)
MgO_3Ca = getEnergy(13)
MgO_4Ca = getEnergy(14)

close(10)
close(11)
close(12)
close(13)
close(14)

print *, MgO_0Ca, mgo_1ca, mgo_2ca, mgo_3ca, mgo_4ca

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
