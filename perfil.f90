program perfil_par

!*******************************************************************************
!**********Este programa calcula el perfil del parametro de orden***************
!*******************************************************************************

implicit none
real(kind=8) :: boxx,boxy,delta_y,pi,angy
integer      :: npart,i,bins,biny
real(kind=8) :: s2,s2sum
real(kind=8), dimension(:), allocatable :: pos_x,pos_y,ang_part,s2_array

open (unit=8, file='confout.txt')
read(8,*)
read(8,*)boxx,boxy
read(8,*)
read(8,*)npart
read(8,*)
read(8,*)
read(8,*)
read(8,*)
read(8,*)

allocate(pos_x(npart))
allocate(pos_y(npart))
allocate(ang_part(npart))

do i = 1, npart
   read(8,*) pos_x(i), pos_y(i), ang_part(i)
   angy = pi - ang_part(i) 
enddo

close(8)

bins=20
pi=1.57

allocate(s2_array(bins))
!se da el tamaño del bin
delta_y=boxy/dble(bins)

!se le asiga un bin a cada partícula 
do i = 1, npart
   biny = int(pos_y(i)/delta_y)+1
   if (biny == 0) stop("biny es igual a cero")
   !se calcula el parámetro de orden
   s2 = cos(2*ang_part(i))
   s2_array(biny) = s2_array(biny) + s2 !contabiliza cuantas particulas hay en una caja
enddo

open (unit=21, file='salida.dat')
do i = 1, bins
   write(21,*) i*delta_y, s2_array(i)
enddo
close(21)

deallocate(pos_x)
deallocate(pos_y)
deallocate(ang_part)
deallocate(s2_array)


end program perfil_par
