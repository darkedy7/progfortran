
subroutine Ap(x_1,n,y)
  implicit none
  integer(kind=8) ,intent (in)::n !número de términos
  real (kind=8),intent (in) ::x_1
  real (kind=8), intent (out)::y
  real (kind=8),dimension(100)::r,x !acumulador
  !variables locales
  real(kind=8)::term, partial_sum
  integer::i
  !termino inicial y la suma de las terminos
  term=1.0
  do i=1,100
     x(i)=float(i)/10.0
     write(1,*) x(i), term
  end do
  write(1,*) ''
  r=0
 partial_sum=term
 !ciclo el número de términos
 do i=1,n
    !calcular el factorial
    term=term*x_1/float(i)
    !agrega este término a la suma parcial
    partial_sum=partial_sum + term
    r(i)=partial_sum
    y=r(i)
    end do
  end subroutine Ap
!iniciamos programa  
program Taylor
  implicit none
  real (kind=8)::y
  integer(kind=8) ::i,j,n
  real(kind=8), dimension(100):: x,exp_true
  
  open(1,file='taylor.dat',status='unknown')
  
  exp_true=exp(x)
  do n=1,15,2
     do j=1,100
        x(j)=float(j)
        x(j)=x(j)/10.0
    !llamamos a la subrutina
      call Ap(x(j),n,y)
        write(1,2) x(j) ,y
          2 format(1x,F12.8,F12.8) 
        write(1,*)''
         end do
end do
 close(1)
end program Taylor
  


  !
!! Taylor.f90
!! 
!! Made by (David Eduardo Hernandez Sanchez)
!! Login   <edyhndz7@ltsp161.example.com>
!! 
!! Started on  Thu Dec  7 16:24:59 2017 David Eduardo Hernandez Sanchez
!! Last update Time-stamp: <2010-oct-11.lunes 17:26:15 (calcaneo)>
!

