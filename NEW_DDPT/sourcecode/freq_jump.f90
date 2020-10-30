PROGRAM Freq_Jump
IMPLICIT NONE
CHARACTER                                   :: dummy*120
INTEGER,PARAMETER                           :: dp = selected_real_kind(15,300)
REAL(kind=dp)                               :: interval,frequency_range,min_freq
INTEGER                                     :: i,j,n,io,min_mode,max_mode,counter
REAL(kind=dp),DIMENSION(:,:),ALLOCATABLE    :: output
REAL(kind=dp),DIMENSION(:),ALLOCATABLE      :: frequencies
LOGICAL                                     :: getoption
    IF(getoption('-s',.true.,dummy)) THEN
        READ(dummy,*) min_mode
    ELSE
        STOP 'No minimum mode number specificied. Declare the minimum mode number by using the -s flag'
    END IF
    
     IF(getoption('-e',.true.,dummy)) THEN
        READ(dummy,*) max_mode
    ELSE
        STOP 'No maximum mode number specificed. Declare the maximum mode number by using the -e flag'
    END IF
    
    OPEN (unit =9874,file='mode.frequencies',status='old',action='read',iostat=io)
    IF (io .ne. 0) STOP 'Error Opening Input File'
    
    n = max_mode -min_mode
    
    ALLOCATE(frequencies(n))

    
    DO i =1,n
        READ (9874, fmt=*,iostat=io) frequencies(i)
        
        IF (io < 0) THEN
            EXIT
        ELSE IF (io > 0) THEN
            STOP 'Error in reading input file'
        END IF
    END DO
    CLOSE(unit=9874, iostat=io)
    IF (io .ne. 0) STOP 'Error Closing Input File'
    frequency_range = maxval(frequencies) - minval(frequencies)
    interval = frequency_range / 300
    min_freq = minval(frequencies)
    print*, "INTERVAL:",interval
    print*, "MINIMUM FREQUENCY",min_freq
    print*, "FREQUENCY RANGE:",frequency_range
    ALLOCATE(output(n,2))
    output(1,1) = 1
    output(1,2) = 1
    do i = 2,n
        output(i,1) = i
        output(i,2) = frequencies(i)/frequencies(i-1)
    end do
    
    DEALLOCATE(frequencies)
    
    OPEN(Unit=3000,File='freq_jump.dat',Status='unknown',action='write',iostat=io)
    if (io .ne. 0) STOP "Error Opening output file"
    do i =1,n
        WRITE(Unit=3000,fmt=*, iostat =io) output(i,1), output(i,2)
    end do
    if( io .ne.0) STOP "Error Writing to the output file"
    CLOSE (Unit=3000,iostat=io)
    if (io .ne. 0) STOP "Error Closing output file"
    DEALLOCATE(output)
    END PROGRAM
    
    FUNCTION getoption(flag,getval,cvalue)
    IMPLICIT NONE
    CHARACTER(*)   :: flag,cvalue
    CHARACTER(160) :: arg
    LOGICAL        :: getoption,getval
    INTEGER        :: l,i,j
  
    getoption=.false.
    i=0
    DO j=1, iargc()
        CALL getarg(j,arg)
        IF (arg .eq. flag) i=j
    END DO
    IF (i .gt. 0) THEN
        getoption=.true.
    END IF
    IF (getval) call getarg(i+1,cvalue)
    END FUNCTION
