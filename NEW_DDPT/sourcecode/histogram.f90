PROGRAM Histogram
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
    interval = frequency_range / 30
    min_freq = minval(frequencies)
    print*, "INTERVAL:",interval
    print*, "MINIMUM FREQUENCY",min_freq
    print*, "FREQUENCY RANGE:",frequency_range
    ALLOCATE(output(30,2))
    do i = 1,30
        output(i,1)=min_freq +interval/2.0 + interval*(i-1)
    end do
    do i = 1,30
        counter = 0
        do j =1,n
            if (((min_freq+(i-1)*interval) <= frequencies(j)) .and.(frequencies(j) <(min_freq + i*interval))) then
                counter = counter + 1
            end if
        end do
        output(i,2) = counter
    end do
    
    DEALLOCATE(frequencies)
    
    OPEN(Unit=1596,File='histogram.dat',Status='unknown',action='write',iostat=io)
    if (io .ne. 0) STOP "Error Opening histogram file"
    do i =1,30
        WRITE(Unit=1596,fmt=*, iostat =io) output(i,1), output(i,2)
    end do
    if( io .ne.0) STOP "Error Writing to the histogram file"
    CLOSE (Unit=1596,iostat=io)
    if (io .ne. 0) STOP "Error Closing histogram file"
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
