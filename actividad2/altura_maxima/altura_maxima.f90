  program altura_maxima
   Implicit none
    !declaramos constantes
     real, parameter::pi=3.1415927
     real, parameter::g=9.8
      !declararemos variables
       real::a 
       real::h , vo
        write(*,*) "calcularemos la altura maxima"
	write(*,*) "no tomaremos en cuenta la resistencia del aire"
	write(*,*) "Dame la velocidad inicial y un ángulo"
	read(*,*) vo , a
	 !convirtiendo el ángulo a radianes
	 a=(a*pi)/180
	 !para calcular la altura utilizaremos
	 !h=((vo**2)sin(a)/2g)
	 h=(vo*vo)*(sin(a)*sin(a))/2*g
	  write(*,*) "h:", h
	  endprogram altura_maxima


        
