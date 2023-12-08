      module day08
      
        type :: t_node
          type(t_node), pointer :: left, right
          character*3 :: name
        end type
      
      contains
      
        subroutine read_line_length(input, length) 
          implicit none
          integer, intent(in)  :: input
          integer, intent(out) :: length
          integer      :: size, idx
          character*10 :: buffer
          
          length = 0
          do
            read(input, '(A)',
     &           advance='NO', eor=99, end=100, size=size) buffer
            length = length + size
          end do
 99       length = length + LEN_TRIM(buffer)
100       rewind(input)
        end subroutine read_line_length
        
        subroutine find_node(input, node, left, right)
          implicit none
          integer    , intent(in)  :: input
          character*3, intent(in)  :: node
          character*3, intent(out) :: left, right
          character*3 :: current
          
          do
            left = ''
            right = ''
            read(input, '(A,4X,A,2X,A,1X)', end=200)
     &        current, left, right
            if (current == node) then
              exit
            end if            
          end do
          
  200     rewind(input)
          read(input, *)
          read(input, *)
        end subroutine find_node
      
      end module day08
      
      program day08_prog
        use day08
        implicit none
        integer :: input_file, size, steps, i
        character*3 :: node, left, right
        character(len=:), allocatable :: instructions
        
        open(newunit=input_file, file='day08.txt')
        call read_line_length(input_file, size)
        allocate(character(size) :: instructions)
        read(input_file, *), instructions
        read(input_file, *)
        
        steps = 0
        node = 'AAA'
  300   do i=1,LEN_TRIM(instructions)
          call find_node(input_file, node, left, right)
          
          if (instructions(i:i) == 'L') then
            node = left
          else
            node = right
          end if
          
          steps = steps + 1

          if (node == 'ZZZ') then
            goto 400
          end if          
        end do
        goto 300
  400   print *, 'part1=', steps
      
99999 end program day08_prog

