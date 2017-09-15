program x_max
 implicit none
 !declaramos constantes
 real, parameter:: pi=3.1415927
 real, parameter:: g=9.8
 real, parameter:: u=45
 !declaramos variables
 real::vo,d,a
 !utilizaremos el ángulo como 45°
 write(*,*) "calcularemos distancia maxima (x) de un objeto"

 a=u*pi/180
 !la fórmula para calcular la distancia máxima es d=((vo*vo)/g)*sin2u
 write(*,*) "dame el valor de la velocidad inicial"
 read(*,*) vo
 d=((vo*vo)/g)*sin(2*a)
 write(*,*) "d: ",d,""
 endprogram x_max
