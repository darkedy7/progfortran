 !----------- Begin ------------
!taylor.f90
program taylor

    implicit none                  
real (kind=8) :: x, exp_true, y
    real (kind=8), external :: exptaylor
    integer :: n

    n = 20               ! number of terms to use
    x = 1.0
    exp_true = exp(x)
    y = exptaylor(x,n)   ! uses function below
    print *, "x = ",x
    print *, "exp_true  = ",exp_true
    print *, "exptaylor = ",y
    print *, "error     = ",y - exp_true

end program taylor

!==========================
function exptaylor(x,n)
!==========================
    implicit none

    ! function arguments:
    real (kind=8), intent(in) :: x
    integer, intent(in) :: n
    real (kind=8) :: exptaylor

    ! local variables:
    real (kind=8) :: term, partial_sum
    integer :: j

    term = 1.
    partial_sum = term

    do j=1,n
        ! j'th term is  x**j / j!  which is the previous term times x/j:
        term = term*x/j   
        ! add this term to the partial sum:
        partial_sum = partial_sum + term   
        enddo
     exptaylor = partial_sum  ! this is the value returned
end function exptaylor
! --------  End -------------
!
!! ex.f90
!! 
!! Made by (David Eduardo Hernandez Sanchez)
!! Login   <edyhndz7@ltsp153.example.com>
!! 
!! Started on  Fri Dec  1 14:03:19 2017 David Eduardo Hernandez Sanchez
!! Last update Time-stamp: <2010-oct-11.lunes 17:26:15 (calcaneo)>
!
