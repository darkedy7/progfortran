 
! sum.f90
! Performs summations using in a loop using EXIT statement
! Saves input information and the summation in a data file

program summation
implicit none
integer ::  a
real:: sum, n,am,hm,m
print*, "This program performs summations. Enter 0 to stop."
open(unit=10, file="SumData.DAT")
!dividir enteros entre enteros
!nunca combinar números
sum = 0.0
n=0.0
m=0.0
do 
 print*, "Add:"
 read*, a
 if (a == 0) then
  exit
else
    sum = sum + a
    n=(n+1)
   m=m+1.0/a
end if
   am=sum/n
   hm=n/m
 write(10,*) a
end do
   

   print*, "Summation =", sum
   print*, "Arithmetic mean =", am
   print*,"Harmonic mean=" ,hm
write(10,*) "Summation =", sum
write(10,*) "Arithmetic mean=", am
write(10,*) "Harmonic mean=", am
   
close(10)

end!
!! medias.f90
!! 
!! Made by (David Eduardo Hernandez Sanchez)
!! Login   <edyhndz7@ltsp106.example.com>
!! 
!! Started on  Mon Oct 30 11:56:41 2017 David Eduardo Hernandez Sanchez
!! Last update Time-stamp: <2010-oct-11.lunes 17:26:15 (calcaneo)>
!

