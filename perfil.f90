program perfil_par

!*******************************************************************************
!**********Este programa calcula el perfil del parametro de orden***************
!*******************************************************************************

implicit none
real(kind=8) :: boxx,boxy,delta_y,pi,angy
integer      :: npart,i,bins,biny
real(kind=8) :: s2
real(kind=8), dimension(:), allocatable :: pos_x,pos_y,ang_part,s2_array
integer, dimension(:), allocatable :: t_bin

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

pi=3.141592653589793
do i = 1, npart
   read(8,*) pos_x(i), pos_y(i), ang_part(i)
   ang_part(i) = pi/2.0 - ang_part(i) 
enddo

close(8)

bins=80
allocate(t_bin(bins))
allocate(s2_array(bins))

s2_array(i) = 0.0
t_bin = 0

!se da el tamaño del bin
delta_y = boxy/dble(bins)
!write(*,*)delta_y

!se le asiga un bin a cada partícula 
do i = 1, npart
   biny = int(pos_y(i)/delta_y)+1
   if (biny == 0) stop("biny es igual a cero")
   !se calcula el parámetro de orden
   s2 = cos(2*ang_part(i))
   s2_array(biny) = s2_array(biny) + s2 !suma el parametro de orden de las particulas dentro de un bin
   t_bin(biny) = t_bin(biny) + 1  !contabiliza el numero de particulas dentro de un bin
enddo

s2_array(:) = s2_array(:)/t_bin(:) !saca el parametro de orden dentro de un bin

!cerca del eje x casi no vamos a encontrar particulas
do i=1,bins 
if (t_bin(i)==0)then 
s2_array(i)=0 
endif
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
deallocate(t_bin)

end program perfil_par
