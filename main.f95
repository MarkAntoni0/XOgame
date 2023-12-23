program xogame 
    implicit none
    
    integer :: i, move, turn
    character(1), dimension(3,3) :: xo_game
    character(1) :: winner, replay
    logical :: over, chkplay
    
    do
      write(*,*) "Play XO Game. Enter 1-9 to play:"
      write(*,*) " "
      write(*,*) " 1 | 2 | 3 "
      write(*,*) "---+---+---"
      write(*,*) " 4 | 5 | 6 "
      write(*,*) "---+---+---"
      write(*,*) " 7 | 8 | 9 "
      write(*,*) " "
    
      call boardsetup(xo_game)
    
      do
        do 
          turn = mod(turn, 2) + 1  ! Alternating turns between players 1 and 2
          write(*,*) "Player ", turn, "'s move? "
          read(*,*) move
          if (move < 1 .or. move > 9) then
            write(*,*) "Invalid input."
            cycle
          else if (chkplay(xo_game, move)) then
            exit
          else 
            write(*,*) "Invalid move, box already occupied."
            cycle
          end if
        end do
    
        if (turn == 1) then
          xo_game((move - 1) / 3 + 1, mod(move - 1, 3) + 1) = "x"
        else
          xo_game((move - 1) / 3 + 1, mod(move - 1, 3) + 1) = "o"
        end if
    
        do i=1,3
          write(*, *) " ", xo_game(i,1), " | ", xo_game(i,2), " | ", xo_game(i,3)
          if (i < 3) write(*,*) "---+---+---"
        end do
    
        call chkovr(xo_game, over, winner)
        if (over) exit
      end do
    
      write(*,*) "The game is over!"
      if (winner == "d") then
        write(*,*) "The game is a draw."
      else
        write(*,*) "The winner is: ", winner
      end if
    
      do
        write(*,*) "Play again? (y/n)"
        read(*,*) replay
        if (replay == "y" .or. replay == "n") exit
        write(*,*) "Invalid input."
      end do
    
      if (replay == "n") exit
    end do
    end
    
    ! Subroutine to set up the XO Game board.
    ! =========================================
    subroutine boardsetup(xo_game)
    implicit none
    integer :: i, j
    character(1), dimension(3,3) :: xo_game
    
    do i = 1,3
      do j = 1,3
        xo_game(i,j) = " "
      end do
    end do
    return
    end
    
    ! Subroutine to check human play.  
    ! =========================================
    logical function chkplay(xo_game, move)
    character(1), dimension(3,3) :: xo_game
    integer :: move
    
    chkplay = .false.
    if (xo_game((move - 1) / 3 + 1, mod(move - 1, 3) + 1) == " ") chkplay = .true.
    end
    
    ! Subroutine to check if the game is over.    
    ! =========================================
    subroutine chkovr(xo_game, over, winner)
    character(1), dimension(3,3) :: xo_game
    character(1) :: winner
    logical :: over, same, dsame
    integer :: ir, ic
    character(1), parameter :: blank = " ", draw = "d"
    
    ! Assume game is over at start.
    over = .true.
    
    ! Check for a winner.
    ! Check rows for a winner.
    do ir = 1, 3
      if (same(xo_game(ir,1),xo_game(ir,2),xo_game(ir,3))) then
        winner = xo_game(ir,1)
        return
      end if
    end do
    ! No winner by rows, check columns for a winner.
    do ic = 1, 3
      if (same(xo_game(1,ic),xo_game(2,ic),xo_game(3,ic))) then
        winner = xo_game(1,ic)
        return
      end if
    end do
    ! No winner by rows or columns, check diagonals.
    dsame = same(xo_game(1,1),xo_game(2,2),xo_game(3,3)) .or. same(xo_game(1,3),xo_game(2,2),xo_game(3,1)) 
    if (dsame) then
      winner = xo_game(2,2)
      return
    end if
    ! No winner at all. See if the game is a draw.
    ! Check each row for an empty space.
    do ir = 1,3
      do ic = 1,3
        if (xo_game(ir,ic) == blank) then
          over = .false.
          return
        end if
      end do
    end do
    
    ! No blank found, game is a draw.
    winner = draw
    
    return    
    end
    
    ! Function to check if three elements in a row, column, or diagonal
    ! are the same.
    ! =========================================
    logical function same(t1,t2,t3)
    character :: t1,t2,t3
    
    if (t1 == "x" .and. t2 == "x" .and. t3 == "x") then
      same = .true.
      return
    else if (t1 == "o" .and. t2 == "o" .and. t3 == "o") then
      same = .true. 
    else
      same = .false.
    end if
end
