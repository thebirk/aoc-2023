      program day04
      implicit none
      integer :: input_file
      character*116 :: line
      character*32 :: card_string
      integer :: card, i, j, matches, sum_part1, score
      integer, dimension(10) :: winning
      integer, dimension(25) :: rolls
      integer, dimension(197) :: cards
      
      matches = 0
      sum_part1 = 0
      cards = 0
      open(newunit=input_file, file='day04.txt')
      
      do
        read(input_file, '(A)', end=9999) line
        call parse_card(line, card, winning, rolls)
        
        do i=1, SIZE(rolls)
          do j=1, SIZE(winning)
            if (rolls(i) == winning(j)) then
              matches = matches + 1
              exit
            end if
          end do
        end do
     
        score = 2**(matches-1)
        sum_part1 = sum_part1 + score
        
        cards(card+1:card+matches) = 
     &    cards(card+1:card+matches)+1+cards(card)
        cards(card) = cards(card) + 1
        
c       print *, 'card=', card, 'matches=', matches,
c    &    'score=', score, 'cards(card)=', cards(card)
     
        matches = 0
      end do
      
9999  print *, 'part1=', sum_part1
      print *, 'part2=', SUM(cards)
      
      end program day04
      
      subroutine parse_card(line, card, winning, rolls)
        implicit none
        character*116         , intent(in)  :: line
        integer               , intent(out) :: card
        integer, dimension(10), intent(out) :: winning
        integer, dimension(25), intent(out) :: rolls
        character*8                         :: card_string
        character*8                         :: dummy
        
        read(line, *) 
     &    dummy, card_string, winning, dummy, rolls
        card_string(LEN_TRIM(card_string):LEN_TRIM(card_string)) = ''
        read(card_string, *) card
      end subroutine parse_card