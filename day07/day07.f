      module day07
      contains

      function card_value(card) result(value)
        implicit none
        character, intent(in) :: card
        integer :: value

        select case(card)
          case('A')
            value = 13
          case('K')
            value = 12
          case('Q')
            value = 11
          case('J')
            value = 10
          case('T')
            value = 9
          case('9')
            value = 8
          case('8')
            value = 7
          case('7')
            value = 6
          case('6')
            value = 5
          case('5')
            value = 4
          case('4')
            value = 3
          case('3')
            value = 2
          case('2')
            value = 1
        end select

      end function card_value

      function hand_value(hand) result(value)
        implicit none
        character*5, intent(in) :: hand
        integer :: value, counts(13), i, pairs
        logical :: five, four, three

        counts = 0
        do i=1,LEN(hand)
          counts(card_value(hand(i:i))) = 
     &      counts(card_value(hand(i:i))) + 1
        end do

        five = .FALSE.
        four = .FALSE.
        three = .FALSE.
        pairs = 0
        do i=1,SIZE(counts)
          select case(counts(i))
            case(5)
              five = .TRUE.
            case(4)
              four = .TRUE.
            case(3)
              three = .TRUE.
            case(2)
              pairs = pairs + 1
          end select
        end do

        if (five) then
          value = 7
        else if (four) then
          value = 6
        else if (three .AND. pairs == 1) then
          value = 5
        else if (three) then
          value = 4
        else if (pairs == 2) then
          value = 3
        else if (pairs == 1) then
          value = 2
        else
          value = 1
        endif
      end function hand_value

      function compare_hand(a, b) result(d)
        implicit none
        character*5, intent(in) :: a, b
        integer :: d, i

        d = 0
        do i=1,LEN(a)
          if (a(i:i) /= b(i:i)) then
            d = card_value(a(i:i)) - card_value(b(i:i))
            exit
          end if
        end do
      end function compare_hand

      subroutine bubblesort_hand(hand)
        implicit none
        character*5, intent(inout) :: hand
        character :: swap
        integer :: n, new_n, i

        n = LEN(hand)
        do
          new_n = 0

          do i=2,n
            if (card_value(hand(i-1:i-1)) > card_value(hand(i:i))) then
              swap = hand(i-1:i-1)
              hand(i-1:i-1) = hand(i:i)
              hand(i:i) = swap
              new_n = i
            end if
          end do

          n = new_n

          if (n <= 1) then
            goto 200
          end if
        end do
  200 end subroutine bubblesort_hand

      subroutine bubblesort_hands(hands, bids)
        implicit none
        character*5, allocatable, intent(inout) :: hands(:)
        integer, allocatable, intent(inout) :: bids(:)
        integer :: i, n, new_n, d, swap_bid
        character*5 :: swap_hand

        n = SIZE(hands)
        do
          new_n = 0

          do i=2,n
            d = hand_value(hands(i - 1)) - hand_value(hands(i))
            if (d == 0) then
              d = compare_hand(hands(i - 1), hands(i))
            end if

            if (d > 0) then
              swap_hand = hands(i - 1)
              hands(i - 1) = hands(i)
              hands(i) = swap_hand
              swap_bid = bids(i - 1)
              bids(i - 1) = bids(i)
              bids(i) = swap_bid
              new_n = i
            end if
          end do

          n = new_n
          if (n <= 1) then
            goto 300
          end if
        end do
  300 end subroutine bubblesort_hands

      end module day07
      
      program day07_prog
        use day07
        implicit none
        integer(8) :: input_file, num_hands, i, part1_sum
        character*5, allocatable :: hands(:)
        integer, allocatable :: bids(:)

        open(newunit=input_file, file='day07.txt')

        num_hands = 0
        do
          read(input_file, *, end=100)
          num_hands = num_hands + 1
        end do

  100   rewind(input_file)
        allocate(hands(num_hands))
        allocate(bids(num_hands))

        do i=1,num_hands
          read(input_file, *) hands(i), bids(i)
        end do

        call bubblesort_hands(hands, bids)
        part1_sum = 0
        do i=1,SIZE(hands)
          part1_sum = part1_sum + bids(i) * i
C         print *, 'hand=', hands(i), 'rank=', i, 'bid=', bids(i)
        end do

        print *, 'part1=', part1_sum

      end program day07_prog