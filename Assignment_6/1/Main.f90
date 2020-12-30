program Main 

  implicit none

  integer :: u,i,j
  real(8), dimension (4,3) :: M
  real(8), dimension (3,3) :: N
  real(8), dimension (4,3) :: O

  u=50

  open(u,file='M.dat',status='old')
   do i=1,4
     read(u,*) (M(i,j),j=1,3)
   enddo
  close(u)

 open(u,file='N.dat',status='old')
       do i=1,3
     read(u,*) (N(i,j),j=1,3)
   enddo

  close(u)
  
! do i = 1,4
 !   do j = 1,3
  !  O(i,j) = M(i,1)*N(1,j)+M(i,2)*N(2,j)+M(i,3)*N(3,j)
!end do
!end do
!O = matmul(M,N)
call Matrix_multip(M,N,O)
open(unit=u,file='MN.dat',status='replace')
do i = 1,4
write(u,*) (O(i,j),j=1,3)
end do
close(u)

end program Main
