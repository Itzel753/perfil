program perfil_par

!*******************************************************************************
!**********Este programa calcula el perfil del parametro de orden***************
!*******************************************************************************

implicit none
real      :: boxx_1,boxy_1,boxx_2,boxy_2,delta_y,pi,angy
integer   :: npart,i,bins,ii,biny
double precision :: s2,s2sum
double precision, dimension(:),allocatable :: pos_x,pos_y,ang_part,ex,ey,s2_array

open (unit=8, file='confout.txt')

read(8,*)
read(8,*)boxx_2,boxy_2
!write(*,*)boxx_2,boxy_2
read(8,*)
read(8,*)npart
read(8,*)
read(8,*)
read(8,*)
read(8,*)
read(8,*)
!write(*,*)boxx_2,boxy_2,npart

bins=20
pi=1.57

allocate(pos_x(npart))
allocate(pos_y(npart))
allocate(ang_part(npart))
allocate(s2_array(bins))

!s2_array=0.d0
do i=1,npart
   read(8,*)pos_x(i),pos_y(i),ang_part(i)
!write(1,*)pos_x(i),pos_y(i),ang_part(i)

angy= pi - ang_part(i) 
enddo

open (unit=21, file='salida.dat')

boxx_1=0.0
boxy_1=0.0

!se da el tamaño del bin
delta_y=(boxy_2-boxy_1)/dble(bins)

!se le asiga un bin a cada partícula 
do i=1,npart
biny=int((pos_y(i)+abs(boxy_1))/delta_y)+1
if(biny.eq.0)stop("biny es igual a cero")

!write(*,*)s2_array(biny)
!se calcula el parámetro de orden
s2=cos(2*ang_part(i))

s2_array(biny) =s2_array(biny)+s2 !contabiliza cuantas particulas hay en una caja
enddo

!do ii=1,bins
!  write(21,*) s2_array(ii),ii*delta_y
!enddo

do ii=1,bins
  write(21,*) ii*delta_y,s2_array(ii)
enddo

deallocate(pos_x)
deallocate(pos_y)
deallocate(ang_part)
deallocate(s2_array)

close(8)
close(33)
close(21)

end program perfil_par
