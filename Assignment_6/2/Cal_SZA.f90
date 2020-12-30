program Cal_SZA
use Declination_angle
use AST

implicit none

real(8) :: n1, delta1,lon,lat,hour,minu,a,lst1,ast1,H,SZA,SZA1

write(*,*) 'Input latitude'
read(*,*) lat
write(*,*) 'Input longitude'
read(*,*) lon
write(*,*) 'Input hour '
read(*,*) hour
write(*,*) 'Input min '
read(*,*) minu
write(*,*) 'Input N, which is the number of days that have passed in the year'
read(*,*) n1

lst1 = hour + minu/60
a= 3.1415926536/180

call cal_angle(n1,delta1)
call cal_ast(lst1,n1,lon,ast1)


H = (ast1*60 - 720)/4
SZA1 = cos(lat*a)*cos(delta1*a)*cos(H*a) + sin(lat*a)*sin(delta1*a)
SZA = ACOS(SZA1)/a

write(*,*) SZA

end program Cal_SZA



