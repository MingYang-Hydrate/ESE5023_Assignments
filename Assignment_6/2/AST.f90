module AST
implicit none
real(8) :: LSTM, D, Long, ET, AST0, LST, N
real, parameter :: pi = 3.1415926536
contains

subroutine cal_ast(LST,N,Long,AST0)

implicit none

real(8) :: LSTM, D, Long, ET, AST0, LST, N
real, parameter :: pi = 3.1415926536

D= 360*(N-81)/365
ET= 9.87*sin(2*D*(pi/180))-7.53*cos(D*(pi/180))-1.5*sin(D*(pi/180))
LSTM = 15*floor(Long/15)
AST0 = LST+(4*(LSTM - Long)+ET)/60

end subroutine cal_ast

end module AST


