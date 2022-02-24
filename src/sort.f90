!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE SORT(Xout,N)
  IMPLICIT NONE
  INTEGER::N,i,j
  REAL(8),intent(inout)::Xout(N)
  REAL(8)::a
  DO j=2,N
     a=Xout(j)
     DO i=j-1,1,-1
        IF(Xout(i)<a)EXIT 
        Xout(i+1)=Xout(i)
     ENDDO
     Xout(i+1)=a
  ENDDO
END SUBROUTINE SORT
