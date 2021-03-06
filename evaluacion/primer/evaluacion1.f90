program sphere

! Calculate the surface area of a cylinder.
!
! Declare variables and constants.
! constants=pi
! variables=radius squared 

  implicit none    ! Require all variables to be explicitly declared

  integer :: ierr
  character(1) :: yn
  real :: radius, area , volume
  real, parameter :: pi = 3.141592653589793

  interactive_loop: do

!   Prompt the user for radius and height
!   and read them.

    write (*,*) 'Enter radius.'
    read (*,*,iostat=ierr) radius

!   If radius and height could not be read from input,
!   then cycle through the loop.

    if (ierr /= 0) then
      write(*,*) 'Error, invalid input.'
      cycle interactive_loop
    end if

!   Compute area.  The ** means "raise to a power."

    area = 4*pi*radius*radius

    volume=(4./3.)*(pi*radius*radius*radius)

!   Write the input variables (radius, height)
!   and output (area) to the screen.

    write (*,'(1x,a7,f20.6,5x,a7,f20.6,5x,a7,f20.6)') &
      'radius=',radius,'area=',area, 'volume=', volume

    yn = ' '
    yn_loop: do
      write(*,*) 'Perform another calculation? y[n]'
      read(*,'(a1)') yn
      if (yn=='y' .or. yn=='Y') exit yn_loop
      if (yn=='n' .or. yn=='N' .or. yn==' ') exit interactive_loop
    end do yn_loop

  end do interactive_loop

end program sphere
