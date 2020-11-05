function getEnergy(unit, check) !This function extracts the required energy value from a specified output file
integer, intent(in) :: unit
logical, intent(in) :: check
integer, parameter :: dp = selected_real_kind(15,300)
real(kind=dp) :: getEnergy
integer :: i, k, ipos = 1
character(len=51) :: temporary

!unit : The unit of the output file to extract the energy value from. Passed in to the function
!check : A logical that is true if the program should look for the Gibbs free energy rather than internal energy. Passed in to the function
!getEnergy : Real variable that stores the required energy value when it has been found
!i, k : Counters for loops
!ipos : When searching a string for a specific character using the scan function, this stores its location in the string.
!temporary : Where each line from the output file is read to

!It is not known exactly where the energy value will be in the output file due to users having the ability to add more keywords to the input file,
!producing a varying output file. Therefore it is necessary to scan the lines of text of the output file for what we are looking for, rather than jumping
!directly to a line.
!What we are looking for is either the words 'Final energy' or the words 'Final free energy'. The below portion of code finds whichever is required in the output file.

!It is not necessary to check every single line - the energy value will not be near the beginning
do i= 1, 200 !This dummy loop skips 200 lines down the output file, to prevent the program having to perform if statements on too many lines
 read(unit, '(A)' ) temporary
end do

!Each line from here on will be checked whether it contains the characters 'Final energy' or 'Final free energy'
do k=1, 1500 !Checking 1500 lines ensures that the energy value will be found - it should be found well before the end of the loop, having 1500 is a precaution
 read(unit, '(A)' ) temporary !Pull line from input file, store in temporary variable
 ipos = scan(temporary, "F") !Search for character 'F' in line (case sensitive), store location in variable ipos
 if (ipos.eq.3) then !If the character 'F' is the third character on the line, continue checking line, else, move onto next line. Only 1 if statement performed on each unwanted line
  ipos = scan(temporary, "i") !Search for character 'i' in line
  if (ipos.eq.4) then !If the character 'F' is the fourth character (ie now the program knows the string reads at least'  Fi') continue
   ipos = scan(temporary, "n")
   if (ipos.eq.5) then
    ipos = scan(temporary, "e")
    if (ipos.eq.9) then
     ipos = scan(temporary, "y")
     if (ipos.eq.14) then !By this point the program knows the line reads at least '  Fin   e    y'. It can be safely said this is the line we're looking for
      read(temporary(20:32), *) getEnergy !Store the number found later in the line under getEnergy
      exit !Energy value has been found, exit loop and function
     end if
    else if ((ipos.eq.11).and.(check)) then !This is still checking the location of 'e'. This only applies when looking for the free energy, as the line we are looking for
     !will have different ordering now compared to the regular energy line ('Final free energy' - the first e is located in a different place)
     ipos = scan(temporary, "y")
     if (ipos.eq.19) then
      read(temporary(25:37), *) getEnergy !Found location of free energy line. Retrieve energy and exit loop and function
      exit
     end if
    end if
   end if
  end if
 end if
end do
!The scan function in fortran can only find the position of the first use of a character in a string.
!Searching for 'e' in a string like 'energy' will only tell us the location of the first 'e'.
!This is why the program only searches for certain letters in the line
end function

!This program will read the energies from the GULP output files and plot them on a graph
program readEnergies
implicit none
integer :: istat, j, istat2, istat3, ipos
integer, parameter :: dp = selected_real_kind(15,300)
real(kind=dp) :: MgO_0Ca, MgO_1Ca, MgO_2Ca, MgO_3Ca, MgO_4Ca
real(kind=dp), external :: getEnergy
character (len=100) :: firstline
logical :: free=.false.

!istat, istat2, istat3 : Used for checking errors when opening/reading/writing to files
!j : Counter for loops
!MgO_0Ca, MgO_1Ca etc. : This is where the energy values are held for each ratio of Mg to Ca when extracted from the output file
!getEnergy : Function called upon to get energy values from output file
!firstline : Holds the first line of the input file. Used to know what keywords have been entered
!free : Logical that will be true when looking for the Gibbs free energy


!This section of code looks at the input file to determine whether to look for the Gibbs free energy in the output file
open(30, file = 'MgO.gin', status = 'old', iostat = istat)
if (istat.ne.0) then
 print *, 'Error opening output file'
end if
read(30, '(A)' ) firstline !Read first line from file and store under firstline
close(30)

!Here we are looking for the set of characters 'free_energy' in the first line. Due to the limitations of the scan function, a similar
!method as above has been chosen to look for this
!Since the user can place the keyword anywhere on the first line however, a more rigorous approach is needed. Scanning the line like before would
!not account for the keyword being at the end of the line or other places.

!The program will iterate the start point of the scan of the first line e.g. consider the entire first line (100 characters), then if not found move the start point over
!one space to consider only the rightmost 99 characters, then 98. Each of these times the program will check to see whether the first set of characters are the keyword
!'free_energy'
!If the keyword is found, the program will change a logical accordingly and exit the loop
!If the keyword is not found, the program will calculate the regular energy

do j = 1, 89 !Allowance of 100 characters in first line - 89 + (11 from keyword)
 ipos = scan(firstline(j:89), "f") !If statements check for keyword 'free_energy' for current iteration
 if (ipos.eq.1) then
  ipos = scan(firstline(j:89), "r")
  if (ipos.eq.2) then
   ipos = scan(firstline(j:89), "e")
   if (ipos.eq.3) then
    ipos = scan(firstline(j:89), "y")
    if (ipos.eq.11) then !If the 'free_energy' keyword has been found
     free = .true. !Change free logical to true - will be passed in to getEnergy function
     print *, 'Free energy keyword detected'
     print *, 'Calculating using free energy'
     exit
    end if
   end if
  end if
 end if
!If keyword has not been found, iterate (move starting position of line over) and check again
end do

!Open output files under units 10, 11 etc)
open(10,file = 'MgO_0Ca.gout', status = 'old', iostat = istat )
open(11,file = 'MgO_1Ca.gout', status = 'old', iostat = istat )
open(12,file = 'MgO_2Ca.gout', status = 'old', iostat = istat )
open(13,file = 'MgO_3Ca.gout', status = 'old', iostat = istat )
open(14,file = 'MgO_4Ca.gout', status = 'old', iostat = istat )
if (istat.ne.0) then
 print *, 'Error opening output file'
end if
!Use function to extract energy for each ratio of Mg to Ca. free is passed so the program knows whether it
!should look for the Gibbs free energy or not
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
!Print energy values to terminal - just for testing purposes
print *, MgO_0Ca, MgO_1Ca, MgO_2Ca, MgO_3Ca, MgO_4Ca

!Write the extracted energies to a file called 'energies.dat' along with their corresponding number of Ca atoms in cell
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
!Use plot.plt to plot these results as a graph
call execute_command_line('gnuplot -p plot.plt')

end program
