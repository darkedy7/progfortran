Program Leibniz
  implicit none
!declaramos variables
integer::i,n !n será el valor máximo al que se quiere calcular
real::S,pi

print*, "dame al valor de n, donde n es el número de términos máximos"
read*, n

S=0
do i=0,n
   S=S+(((-1)**i)/(2*float(i)+1))
end do

pi=S*4


print*,  'n=',n, 'pi=',pi

 end program Leibniz
