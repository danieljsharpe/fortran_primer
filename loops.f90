! Use goto to escape nested loops, since EXIT will only exit from the internal loop

loop1: do i = 1, 100
loop2: do j = 1, 100
if (i .lt. 1) then
goto 40
end do loop1
end do loop2

40 print *, "I have escaped!"
! Note the named do loops. Can use these with keywords for loop control e.g.
cycle loop1
exit loop1
!etc.

! Implied DO loops for array initialisation
integer :: a(10) = (/(j,j=1,10)/)
!works in write and data statements as well as initialisation statements
write (*,6) (a(j),j=1,10)
data (a(j),j=1,10)/10*8/

! Nested implied DO loops
WRITE(*,100)((MATRIX(I,J),J=1,8),I=1,5)
!Using a FORMAT statement labelled 100, the WRITE statement prints out 40 array elements in the order MATRIX(1,1), MATRIX(1,2), …,
! MATRIX(1,8), MATRIX(2,1), MATRIX(2,2), …, MATRIX(2,8), …, MATRIX(5,1), MATRIX(5,2), …, MATRIX(5,8
