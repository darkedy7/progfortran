subroutine expD(x,x1,n)
  real(kind=8), intent(in)::x
  real(kind=8),dimension(100), intent(out)::x1
  integer, intent(in)::n
  !variables
  real(kind=8):: term,partial_sum
  integer::i

  term=1.
  partial_sum=term
  do i=1,j
     term=term*x/i
     partial_sum=partial_sum+term
  end do
  x1=partial_sum
end subroutine expD


program Taylor
  implicit none
  real(kind=8) :: x,term,partial_sum,exp_true
  real(kind=8),dimension(100):: x1
  integer ::n,j
  open(unit=1, file='taylor.dat',status='unknown')
  
do j=1,15,2
     x=float(j)
     call  expD(x,x1,n)
     exp_true=exp(x)
 
  print*, "x=", x1
  print*, "exp_true = " , exp_true
  print*, "error=", x1-exp_true
  write(1,*) x1 , exp_true
   end do

end program Taylor
  !
!! SR2.f90
!! 
!! Made by (David Eduardo Hernandez Sanchez)
!! Login   <edyhndz7@ltsp153.example.com>
!! 
!! Started on  Fri Dec  1 11:57:05 2017 David Eduardo Hernandez Sanchez
!! Last update Time-stamp: <2010-oct-11.lunes 17:26:15 (calcaneo)>
!
