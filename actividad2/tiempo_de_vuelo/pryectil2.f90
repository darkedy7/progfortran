program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927
  real:: t

  ! definimos las variables
  real :: a
  real :: vo

  write(*,*) "dame la velocidad inicial y  el ángulo"
  read(*,*) vo, a

  !convirtiendo el ángulo a radianes
   a=a*pi/180.0

  !para calcular el tiempo utilizaremos
  !t=(2vo*sin(a))/g

  print*, "no se tomó  el cuenta la resistencia del aíre"
   
    t=((2*vo*sin(a))/g)

       write(*,*) "t: ",t


 endprogram projectile

  
  
   



  
