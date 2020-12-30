subroutine Matrix_multip(M,N,O)

implicit none

integer:: i, j
real(8), dimension(4,3):: M
real(8), dimension(3,3):: N
real(8), dimension(4,3):: O

do i = 1,4
   do j = 1,3
    O(i,j) = M(i,1)*N(1,j)+M(i,2)*N(2,j)+M(i,3)*N(3,j)
   end do
end do

end
