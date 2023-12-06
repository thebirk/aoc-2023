      program day06
      implicit none
      character*64 :: dummy, dummy2
      integer :: input_file, times(4), distances(4),
     &  i, part1, total_beaten, write_offset
      integer(8) :: time, speed, distance, travel_distance
      
      open(newunit=input_file, file='day06.txt')
      
      read(input_file, *) dummy, times
      read(input_file, *) dummy, distances
      
c     print *, 'times=', times
c     print *, 'distances=', distances
      
      part1 = 1
      
      do i=1,SIZE(times)
        time = times(i)
        distance = distances(i)
        total_beaten = 0
        
        do speed=0,time
          travel_distance = speed * (time - speed)
          
          if (travel_distance > distance) then
            total_beaten = total_beaten + 1
          end if
        end do
        
        part1 = part1 * total_beaten
      end do
      
      print *, 'part1=', part1
      
      write_offset = 1
      dummy = ''
      do i=1,SIZE(times)
        dummy2 = ''
        write(dummy2, *) times(i)
        write(dummy(LEN_TRIM(dummy)+1:), '(A)') TRIM(ADJUSTL(dummy2))
      end do
      read (dummy, *) time
      
      write_offset = 1
      dummy = ''
      do i=1,SIZE(distances)
        dummy2 = ''
        write(dummy2, *) distances(i)
        write(dummy(LEN_TRIM(dummy)+1:), '(A)') TRIM(ADJUSTL(dummy2))
      end do
      read (dummy, *) distance
      
      total_beaten = 0
      
      do speed=0,time
        travel_distance = speed * (time - speed)
        
        if (travel_distance > distance) then
          total_beaten = total_beaten + 1
        end if
      end do
      
      print *, 'part2=', total_beaten
      
      end program day06