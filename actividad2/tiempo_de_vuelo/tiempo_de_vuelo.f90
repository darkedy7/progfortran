program projectile
  implicit none

  ! definimos constantes
  real, parameter :: g = 9.8
  real, parameter :: pi = 3.1415927

  ! definimos las variables
  real :: a
  real :: t, vo
  write(*,*) "calcularemos el tiempo de vuelo de un proyectil  con un ángulo y velocidad inicial"

  write(*,*) "dame la velocidad inicial y  el ángulo"
  read(*,*) vo, a

  !convirtiendo el ángulo a radianes
   a=a*pi/180.0

  !para calcular el tiempo utilizaremos
  !t=(2vo*sin(a))/g

  t=((2*vo*sin(a))/g)

  write(*,*) "t: ", t


  endprogram projectile

  
  
   



  
