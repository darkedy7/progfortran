!desplazamiento en X & Y

 implicit none
 !Declaramos Constantes
 Real,Parameter:: Pi=3.1415927
 Real,parameter:: g=9.8
 Real, parameter:: delt=0.1
 Integer::k
 !declaramos Variables
 Real::t,Vo,u
 real::x   ,y
 

print*, "calcularemos las cordenas del tiro parabólico"
Print*, "dame el ángulo y la Vo"
read*, u, Vo
U=U*pi/180
open(1, file='xy.dat',status='unknown')
Do k=1,100
    t=float(k)*delt
    x =Vo*(cos(u))*t
   y= Vo*sin(u)*t-(0.5)*(g)*(t*t)
     write(1,*) x,y
  end do
  close(1)

      end program
 
