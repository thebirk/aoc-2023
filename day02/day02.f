      program day02
      implicit none
      integer :: input
      character*200 :: line
      integer :: game, sum_valid_game_ids, sum_power_minimum_sets
      integer, dimension(3) :: cubes, max_cubes
      integer :: count_string
      logical :: valid
      game = 0
      sum_valid_game_ids = 0
      sum_power_minimum_sets = 0
      line = ''
      
      open(newunit=input, file='day02.txt')

      do
        read(input, "(A5,A)", end=9999) line, line
        read (line(:INDEX(line, ':')-1), *), game
        line = TRIM(line(INDEX(line, ':')+1:))        
        
c        call parse_game_part1(line, cubes, valid)
c        if (valid) then
c          sum_valid_game_ids = sum_valid_game_ids + game
c        end if
        
        call parse_game_part2(line, cubes, max_cubes)
        sum_power_minimum_sets = sum_power_minimum_sets + 
     &    (max_cubes(1) * max_cubes(2) * max_cubes(3))
      end do
      
c9999  print *, "part1: sum valid game ids=", sum_valid_game_ids
9999  print *,'part2: sum power of minimum sets=',sum_power_minimum_sets

      end program day02
      
      subroutine parse_game_part2(line, cubes, max_cubes)
        implicit none
        character*200        , intent(inout)  :: line
        integer, dimension(3), intent(out)    :: cubes, max_cubes
        character*5, dimension(3), parameter :: ALL_CUBES = [
     &    'red', 'green', 'blue'
     &  ]
        character*200 :: current_line
        character*20  :: current_cube
        integer       :: n, i
        character*5   :: cube_color
        integer       :: count_string
        
        line(LEN_TRIM(line)+1:LEN_TRIM(line)+1) = ';'
        cubes = [0, 0, 0]
        max_cubes = [0, 0, 0]
        
        do
          if (LEN_TRIM(line) == 0) then
            exit
          end if
          
          current_line = line(:INDEX(line, ';')+1)
          line = line(INDEX(line, ';')+1:)
          current_line(LEN_TRIM(current_line):LEN_TRIM(current_line)) =
     &      ','
     
     
          do
            if (LEN_TRIM(current_line) == 0) then
              exit
            end if
          
            current_cube = current_line(:INDEX(current_line, ',')+1)
            current_line = current_line(INDEX(current_line, ',')+1:)
           
            read(current_cube, *) n, cube_color
            
            do i=1, LEN(ALL_CUBES)
              if (TRIM(cube_color) == ALL_CUBES(i)) then
                cubes(i) = n
              end if
            end do
            
            if (cubes(1) > max_cubes(1)) then
              max_cubes(1) = cubes(1)
            else if (cubes(2) > max_cubes(2)) then
              max_cubes(2) = cubes(2)
             else if (cubes(3) > max_cubes(3)) then
              max_cubes(3) = cubes(3)
            end if
          end do
        end do
        
      end subroutine parse_game_part2

      subroutine parse_game_part1(line, cubes, valid)
        implicit none
        character*200        , intent(inout)  :: line
        integer, dimension(3), intent(out)    :: cubes
        logical              , intent(out)    :: valid
        character*5, dimension(3), parameter :: ALL_CUBES = [
     &    'red', 'green', 'blue'
     &  ]
        character*200 :: current_line
        character*20  :: current_cube
        integer       :: n, i
        character*5   :: cube_color
        integer       :: count_string
        
        line(LEN_TRIM(line)+1:LEN_TRIM(line)+1) = ';'
        cubes = [0, 0, 0]
        valid = .TRUE.
        
        do
          if (LEN_TRIM(line) == 0) then
            exit
          end if
          
          current_line = line(:INDEX(line, ';')+1)
          line = line(INDEX(line, ';')+1:)
          current_line(LEN_TRIM(current_line):LEN_TRIM(current_line)) =
     &      ','
     
     
          do
            if (LEN_TRIM(current_line) == 0) then
              exit
            end if
          
            current_cube = current_line(:INDEX(current_line, ',')+1)
            current_line = current_line(INDEX(current_line, ',')+1:)
           
            read(current_cube, *) n, cube_color
            
            do i=1, LEN(ALL_CUBES)
              if (TRIM(cube_color) == ALL_CUBES(i)) then
                cubes(i) = n
              end if
            end do
          end do

          if (cubes(1) > 12) then
            valid = .FALSE.
          else if (cubes(2) > 13) then
            valid = .FALSE.  
           else if (cubes(3) > 14) then
            valid = .FALSE.
          end if
        end do
        
      end subroutine parse_game_part1
      
      function count_string(string, ch) result(c)
        implicit none
        character(len=*), intent(in) :: string
        character       , intent(in) :: ch
        integer :: c, i
        
        c = 0
        
        do i=1, LEN(string)
          if (string(i:i) == ch) then
            c = c + 1
          end if
        end do
        
      end function count_string