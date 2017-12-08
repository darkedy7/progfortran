
subroutine Ts(x_1,n,y)
    implicit none
    integer(kind=8), intent(in) ::n !número de términos
    real (kind=8), intent(in):: x_1
    real(kind=8), intent(out)::y
    real(kind=8), dimension(100)::r,x !acumulador
    !variables locales
    real(kind=8)::term,partial_sum
    integer::i
    !término inicial y suma de términos
    term=1.0
    r=0
    partial_sum=term
    !bucle para el número de términos
    do i=1,n
       !calcular el factorial
       term=((-1)**(i)*((i)+1))/((float(i)*2)+1)
       !agregar el término a la suma parcial
       partial_sum=partial_sum-term
       r(i)=partial_sum
       y=r(i)
    end do
  end subroutine Ts
  !iniciamos programa
  program TaylorS
    implicit none
    real(kind=8),parameter:: pi=3.1415
    real(kind=8)::y
    integer(kind=8) i,j,n
    real(kind=8), dimension(100)::x, true_sin
    open(3,file='AproxSin.dat',status='unknown')
true_sin=sin(pi*x/180)
do n=1,30,2
   do j=1,100
      x(j)=float(j)
      x(j)=x(j)/10.0
      !llamamos a la subrutina
      call TS(x(j),n,y)
      write(3,2) x(j) , y
2     format(2x,F12.8,F12.8)
      write(3,*) ''
   end do
end do
close(3)
end program TaylorS






    
!! Tsin.f90
!! 
!! Made by (David Eduardo Hernandez Sanchez)
!! Login   <edyhndz7@ltsp161.example.com>
!! 
!! Started on  Thu Dec  7 18:54:42 2017 David Eduardo Hernandez Sanchez
!! Last update Time-stamp: <2010-oct-11.lunes 17:26:15 (calcaneo)>
!

