!introducimos una función que calcule X y Y en coordenadas polares
!o sea,X=rcos(theta) y Y=rsin(theta)
!donde r= 1.496d8 ya escrito en DP
!recuerda que estamos utilizando variables de doble precisión
!la función sehace antes del código del programa para llamarla después
!dentro de este

function funcx(theta) result(x)
    double precision, intent(in) ::theta
    double precision             ::x
    x=1.496d9*dcos(theta)
  end function funcx

  function funcy(theta) result(y)
    double precision, intent(in) ::theta
    double precision             :: y
    y=1.496d8*dsin(theta)
  end function funcy
!inciamos el programa
  program orbita
  implicit none
  !declaramos Constantes en DP
  !recuerda quela d(número) indicará la potencia a la que se
  !encuentra el número
  double precision, parameter:: pi=3.1416d0 
  !declaramos variables
  double precision::theta,funcy, funcx
  integer::i
  double precision,dimension(1000):: x, y
open(1, file= 'sol-tierra.dat' , status ='unknown')
  do i=1,360,1
     theta=dble(i)
     theta=(theta*pi)/180.0d0
     x(i)=funcx(theta)
     y(i)=funcy(theta)
     
     write(1,*) x(i), y(i)
     end do
     close(1)
end program orbita 
!
!! orbita.f90
!! 
!! Made by (David Eduardo Hernandez Sanchez)
!! Login   <edyhndz7@ltsp163.example.com>
!! 
!! Started on  Fri Nov 10 11:24:34 2017 David Eduardo Hernandez Sanchez
!! Last update Time-stamp: <2010-oct-11.lunes 17:26:15 (calcaneo)>
!

