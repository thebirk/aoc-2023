      program day01
      implicit none
      character*64 :: line
      integer :: input
      integer :: calibration
      integer :: calibration_sum_part1
      integer :: calibration_sum_part2
      calibration_sum_part1 = 0
      calibration_sum_part2 = 0
      
      open (newunit=input, file="day01.txt")
      
      do
        read(input, *, end=9999) line
        call get_calibration_from_line_part1(line, calibration)
        calibration_sum_part1 = calibration_sum_part1 + calibration
        call get_calibration_from_line_part2(line, calibration)
        calibration_sum_part2 = calibration_sum_part2 + calibration
      end do
      
9999  print *, 'part1 ', 'Sum of all calibration values: '
     &       , calibration_sum_part1
     
      print *, 'part2 ', 'Sum of all calibration values: '
     &       , calibration_sum_part2
   
      end program day01
      
      subroutine get_calibration_from_line_part2(line, calibration)
        implicit none
        character*64, intent(in)  :: line
        integer     , intent(out) :: calibration
        integer, parameter :: ASCII_ZERO = ICHAR('0')
        character*5, dimension(10), parameter :: NUMBERS = [
     &    'zero', 'one', 'two', 'three', 'four',
     &    'five', 'six', 'seven', 'eight', 'nine'
     &  ]
        character    :: c
        character*64 :: line_numbers
        integer      :: line_numbers_offset
        
        calibration = 0
        line_numbers = ''
        line_numbers_offset = 0
        
        do i=1, LEN_TRIM(line)
          c = line(i:i)
          
          if (c.ge.'0' .and. c.le.'9') then
            line_numbers_offset = line_numbers_offset + 1
            line_numbers(line_numbers_offset:line_numbers_offset) = c
          else
            do j=1,10
              if (INDEX(line(i:i+4), TRIM(NUMBERS(j))) == 1) then
                line_numbers_offset = line_numbers_offset + 1
                line_numbers(line_numbers_offset:line_numbers_offset) =
     &            CHAR(ASCII_ZERO + j - 1)
              end if
            end do
          end if
        end do
        
        calibration = 
     &    (ICHAR(line_numbers(1:1))-ASCII_ZERO) * 10 + 
     &    (ICHAR(line_numbers(line_numbers_offset:line_numbers_offset)) 
     &      - ASCII_ZERO)
     
      end subroutine get_calibration_from_line_part2

      subroutine get_calibration_from_line_part1(line, calibration)
        implicit none
        character*64, intent(in)  :: line
        integer     , intent(out) :: calibration
        integer, parameter :: ASCII_ZERO = ICHAR('0')
        character    :: c
        character*64 :: line_numbers
        integer      :: line_numbers_offset
        
        calibration = 0
        line_numbers = ''
        line_numbers_offset = 0
        
        do i=1, LEN_TRIM(line)
          c = line(i:i)
          
          if (c.ge.'0' .and. c.le.'9') then
            line_numbers_offset = line_numbers_offset + 1
            line_numbers(line_numbers_offset:line_numbers_offset) = c
          end if
        end do
        
        calibration = 
     &    (ICHAR(line_numbers(1:1))-ASCII_ZERO) * 10 + 
     &    (ICHAR(line_numbers(line_numbers_offset:line_numbers_offset)) 
     &      - ASCII_ZERO)
      
      end subroutine get_calibration_from_line_part1
