program day04

implicit none
integer :: a,b,c,d, io_stat
integer :: cnt = 0
do
    read (5,*,iostat=io_stat) a,b,c,d
    if (io_stat /= 0) exit
    if (a <= c .and. b >= d .or. &
        a >= c .and. b <= d) then 
        cnt = cnt + 1
    end if
end do
print*, cnt
stop
end

