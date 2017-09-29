!desplazamiento en X & Y

 implicit none
 !Declaramos Constantes
 Real,Parameter:: Pi=3.1415927
 Real,parameter:: g=10
 Real, parameter:: delt=0.1
 Real,parameter::Vo=10
 Integer::k
 Integer::l
 !declaramos Variables
 Real::t,U
 real::x,y
 

print*, "calcularemos las cordenas del tiro parabólico"
open(1, file='final.dat',status='unknown')
do l=15,90,15
   U=float(l)*pi/180.0
   write(1,*) ''

Do k=1,100
    t=float(k)*delt
    x =Vo*(cos(u))*t
    y= Vo*sin(u)*t-(0.5)*(g)*(t*t)
    
    if (y<0.0)  exit
    
     write(1,*) x,y
  end do
  end do
  close(1)

      end program
 
