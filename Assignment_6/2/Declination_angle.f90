module Declination_angle

implicit none

real(8)  :: N, delta
real, parameter :: pi = 3.1415926536

contains

subroutine cal_angle(N,delta)

implicit none

real(8)  :: N, delta
real, parameter :: pi = 3.1415926536

delta = 23.45*sin((N+284)/365*360*(pi/180))

end subroutine cal_angle

end module Declination_angle



