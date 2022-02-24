!!$program main
!!$
!!$implicit none
!!$
!!$real (kind=8), allocatable :: indat(:,:)
!!$integer :: n, i,nrow,ncol, maxlag,firstyear,lastyear,startyear
!!$real (kind=8), allocatable :: cordat(:,:)
!!$real (kind=8), allocatable :: acorrfunc(:,:)
!!$real (kind=8) :: mv
!!$real (kind=8) :: sig,CORR(1:7)
!!$real(kind=8) :: dinvnorm
!!$nrow = 501
!!$ncol = 3
!!$allocate (indat(nrow,ncol))
!!$
!!$startyear = 1500
!!$
!!$! Missing values
!!$mv=-99.99
!!$
!!$! Maximaler Lag
!!$maxlag = 50
!!$
!!$! Alpha
!!$sig = 0.01
!!$
!!$! Daten einlesen
!!$open(1,file="naoi_hw_summer.csv")
!!$do i = 1,nrow
!!$   read(1,*)indat(i,1:ncol)
!!$enddo
!!$
!!$! Auswahl eines Zeitraums
!!$firstyear = 1831
!!$lastyear = 1990
!!$n = lastyear - firstyear + 1
!!$allocate (cordat(1:n,2))
!!$
!!$! Korrelation der NAOI und Hochwasserdaten
!!$!do i = 1,n
!!$   cordat(1:n,1) = indat((firstyear-startyear)+1:(firstyear+n),2)
!!$   cordat(1:n,2) = indat((firstyear-startyear)+1:(firstyear+n),3)
!!$!enddo
!!$
!!$call CORRAUTO(cordat(1:n,1),cordat(1:n,2),n,mv,maxlag,sig,CORR)
!!$
!!$!write(*,"(7f10.4)")CORR(1:7)
!!$
!!$
!!$end program main



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE CORRAUTO(verbose,X,Y,N,MV,MAXLAG,ALPHA, CORR,NEFF_R)
  ! Pearson Correlation between two Variables [X and Y]
  ! taking into account for missing values and
  ! autocorrelation [1 to MAXLAG] in both Variables
  ! estimating Effective Sample Size
  ! and calculating Confidence Intervals [for ALPHA]
  ! assuming original Sample Size [N] and effective sample size [NEFF_R] resp.
  !
  ! Provides the following output [in CORR]
  ! 1) Pearson correlation coefficient
  ! 2) Lower confidence limit for N
  ! 3) Upper confidence limit for N
  ! 4) ALPHA of confidence Interval (if 0 is not included within confidence interval)
  !    -9 if 0 is included within confidence interval
  ! 5) Lower confidence limit for NEFF_R
  ! 6) Upper confidence limit for NEFF_R
  ! 7) ALPHA of confidence Interval (if 0 is not included within confidence interval)
  !    -9 if 0 is included within confidence interval
  !
  ! Additional commented output is provided via console output
  !
  !
  ! christoph.beck@geo.uni-augsburg.de
  !
  ! ACORR field size increased by a.philipp@geo.uni-augsburg.de
  ! avoid NEFF_R > NUSE by a.philipp@geo.uni-augsburg.de December 2012
  !
  implicit none
  integer :: verbose
  integer :: N ! SAMPLE SIZE OF X AND Y [INPUT]
  integer :: MAXLAG ! MAXIMUM LAG FOR CALCULATING AUTOCORRELATION [INPUT]
  real (kind=8) :: MV ! MISSING VALUE INDICATOR FOR X AND Y [INPUT]
  real (kind=8) :: ALPHA ! SIGNIFICANCE LEVEL ALPHA FOR ESTIMATING CONFIDENCE INTERVALS [INPUT]
  real (kind=8) , dimension(N) :: X, Y ! INPUT VARIABLES AS VECTORS OF LENGTH N [INPUT]

  real (kind=8) :: XY(1:N,1:2) ! VECTORS OF LENGTH N [INTERNAL]
  integer :: i,ii, NUSE, LAG ! [INTERNAL]
  real(kind=8) :: MEAN(1:2),SDEV(1:2), COVAR ! [INTERNAL]
  real(kind=8) :: ACORR(1:2,1:MAXLAG+1,1:4) ! [INTERNAL] !MAXLAG+1 instead of MAXlAG!
  real(kind=8) :: dinvnorm,SIGLEV,ZVAL ! [INTERNAL]
  integer :: ctx,cty,ct ! [INTERNAL]
  real(kind=8) :: NEFF_R ! EFFECTIVE SAMPLE SIZE [INTERNAL]
  real(kind=8) :: CORR(1:7) ! RESULTS OF CORRELATION ANALYSIS [OUTPUT]

  XY(1:N,1) = X(1:N)
  XY(1:N,2) = Y(1:N)

  SIGLEV = 1-(ALPHA/2)
  ! Autocorrelation coefficient ACORR
  do ii = 1, 2
     do LAG = 1, MAXLAG
        NUSE = 0
        MEAN = 0.0
        SDEV = 0.0
        COVAR = 0.0
        do i = 1,N-LAG
           if(XY(i,ii) > MV.and.XY(i+LAG,ii) > MV)then
              MEAN(1) = MEAN(1) + XY(i,ii)
              MEAN(2) = MEAN(2) + XY(i+LAG,ii)
              NUSE = NUSE + 1
           endif
        enddo
        MEAN(1) = MEAN(1) / NUSE
        MEAN(2) = MEAN(2) / NUSE
        do i = 1,N-LAG
           if(XY(i,ii) > MV.and.XY(i+LAG,ii) > MV)then
              SDEV(1) = SDEV(1) + (XY(i,ii) - MEAN(1))**2
              SDEV(2) = SDEV(2) + (XY(i+LAG,ii) - MEAN(2))**2
              COVAR = COVAR + ( (XY(i,ii) - MEAN(1)) * (XY(i+LAG,ii) - MEAN(2)) )
           endif
        enddo
        SDEV(1) = SQRT(SDEV(1) / (NUSE - 1))
        SDEV(2) = SQRT(SDEV(2) / (NUSE - 1))
        COVAR = COVAR / (NUSE - 1)
        ACORR(ii,LAG,1) = COVAR / (SDEV(1)*SDEV(2))
        ! Significance of ACORR
        ZVAL = dinvnorm(SIGLEV)
        ACORR(ii,LAG,2) = ACORR(ii,LAG,1) - ZVAL * (1 - ACORR(ii,LAG,1)**2) / SQRT(REAL(NUSE) - 1)
        ACORR(ii,LAG,3) = ACORR(ii,LAG,1) + ZVAL * (1 - ACORR(ii,LAG,1)**2) / SQRT(REAL(NUSE) - 1)
        if(ACORR(ii,LAG,1) < 0 .and. ACORR(ii,LAG,3) < 0)then
           ACORR(ii,LAG,4) = ALPHA
        elseif(ACORR(ii,LAG,1) > 0 .and. ACORR(ii,LAG,2) > 0)then
           ACORR(ii,LAG,4) = ALPHA
        else
           ACORR(ii,LAG,4) = -9
        endif
     enddo
  enddo
  ! Effective Sample Size estimate
  NUSE = 0
  NEFF_R = 0.0
  ctx = 0
  cty = 0
  do i = 1,N
     if(X(i) > MV.and.Y(i) > MV)then
        NUSE = NUSE + 1
     endif
  enddo
  do i = 1,MAXLAG
     if(ACORR(1,i,4) > -9) then
        ctx=ctx+1
     else
        exit
     endif
  enddo
  do i = 1,MAXLAG
     if(ACORR(2,i,4) > -9) then
        cty=cty+1
     else
        exit
     endif
  enddo

  !write(*,*)"ctx,cty :",ctx,cty

  ct = MIN(ctx,cty) + 1 ! <- this could be too large, corrected above
  do i = 1,ct
     NEFF_R = NEFF_R + (ACORR(1,i,1) * ACORR(2,i,1))
     !                          ^              ^
  enddo

  ! At line 166 of file effectivesamplesize.f90
  ! Fortran runtime error: Index '13' of dimension 2 of array 'acorr' above upper bound of 12


  NEFF_R = NUSE / (1+2*NEFF_R)

  ! AVOID NEFF_R being larger than NUSE !
  if(NEFF_R>NUSE)NEFF_R=NUSE

  ! Correlation coefficient CORR
  NUSE = 0
  MEAN = 0.0
  SDEV = 0.0
  COVAR = 0.0
  CORR = 0.0
  do i = 1,N
     if(X(i) > MV.and.Y(i) > MV)then
        MEAN(1) = MEAN(1) + X(i)
        MEAN(2) = MEAN(2) + Y(i)
        NUSE = NUSE + 1
     endif
  enddo
  MEAN(1) = MEAN(1) / NUSE
  MEAN(2) = MEAN(2) / NUSE
  do i = 1,N
     if(X(i) > MV.and.Y(i) > MV)then
        SDEV(1) = SDEV(1) + (X(i) - MEAN(1))**2
        SDEV(2) = SDEV(2) + (Y(i) - MEAN(2))**2
        COVAR = COVAR + ( (X(i) - MEAN(1)) * (Y(i) - MEAN(2)) )
     endif
  enddo
  SDEV(1) = SQRT(SDEV(1) / (NUSE - 1))
  SDEV(2) = SQRT(SDEV(2) / (NUSE - 1))
  COVAR = COVAR / (NUSE - 1)

  if(SDEV(1)*SDEV(2)>0.d0)then
     CORR(1) = COVAR / (SDEV(1)*SDEV(2))
  else
     CORR(1) = COVAR
  endif
  ! Significance of CORR (assuming original sample size)
  ZVAL = dinvnorm(SIGLEV)
  CORR(2) = CORR(1) - ZVAL * (1 - CORR(1)**2) / SQRT(REAL(NUSE) - 1)
  CORR(3) = CORR(1) + ZVAL * (1 - CORR(1)**2) / SQRT(REAL(NUSE) - 1)
  if(CORR(1) < 0 .and. CORR(3) < 0)then
     CORR(4) = ALPHA
  elseif(CORR(1) > 0 .and. CORR(2) > 0)then
     CORR(4) = ALPHA
  else
     CORR(4) = -9
  endif
  ! Significance of CORR (assuming effective sample size)
  CORR(5) = CORR(1) - ZVAL * (1 - CORR(1)**2) / SQRT(REAL(NEFF_R) - 1)
  CORR(6) = CORR(1) + ZVAL * (1 - CORR(1)**2) / SQRT(REAL(NEFF_R) - 1)
  if(CORR(1) < 0 .and. CORR(6) < 0)then
     CORR(7) = ALPHA
  elseif(CORR(1) > 0 .and. CORR(5) > 0)then
     CORR(7) = ALPHA
  else
     CORR(7) = -9
  endif

  ! ########################
  ! Console Output
  if(verbose>3)then
     !write(*,*)"-----------------------------------"
     write(*,*)"Variable X - Autocorrelation Function for lags 1 to ",MAXLAG
     write(*,"(A6,4A12)")"lag","pcorr","ci_low","ci_upp","alpha"
     do i = 1, MAXLAG
        write(*,"(i6,4f12.4)")i,ACORR(1,i,1:4)
     enddo
     write(*,*)
     write(*,*)"Variable Y - Autocorrelation Function for lags 1 to ",MAXLAG
     write(*,"(A6,4A12)")"lag","pcorr","ci_low","ci_upp","alpha"
     do i = 1, MAXLAG
        write(*,"(i6,4f12.4)")i,ACORR(2,i,1:4)
     enddo
     write(*,*)
     write(*,"(A50,i12)")" Original Sample Size = ",N
     write(*,"(A50,i12)")"          Valid cases = ",NUSE
     write(*,"(A50,f12.4)")"Effective Sample Size = ",NEFF_R
     write(*,"(A50,f12.4)")"Alpha value for confidence interval estimates = ",ALPHA
     write(*,*)
     write(*,"(7A12)")"pcorr","ci_low n","ci_upp n","alpha n","ci_low neff","ci_upp neff","alpha neff"
     write(*,"(7f12.4)")CORR(1:7)
     !write(*,"(a)")       "------------------------------------------------------------------------------"
     write(*,"(a)")       "______________________________________________________________________________"
     write(*,*)
  endif
END SUBROUTINE CORRAUTO

!##########################
real*8 function dinvnorm(p)
  ! Quantiles of the standardized normal distribution
  real*8 p,p_low,p_high
  real*8 a1,a2,a3,a4,a5,a6
  real*8 b1,b2,b3,b4,b5
  real*8 c1,c2,c3,c4,c5,c6
  real*8 d1,d2,d3,d4
  real*8 z,q,r
  a1=-39.6968302866538
  a2=220.946098424521
  a3=-275.928510446969
  a4=138.357751867269
  a5=-30.6647980661472
  a6=2.50662827745924
  b1=-54.4760987982241
  b2=161.585836858041
  b3=-155.698979859887
  b4=66.8013118877197
  b5=-13.2806815528857
  c1=-0.00778489400243029
  c2=-0.322396458041136
  c3=-2.40075827716184
  c4=-2.54973253934373
  c5=4.37466414146497
  c6=2.93816398269878
  d1=0.00778469570904146
  d2=0.32246712907004
  d3=2.445134137143
  d4=3.75440866190742
  p_low=0.02425
  p_high=1-p_low
  if(p.lt.p_low) goto 201
  if(p.ge.p_low) goto 301
201 q=dsqrt(-2*dlog(p))
  z=(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/ &
       &((((d1*q+d2)*q+d3)*q+d4)*q+1)
  goto 204
301 if((p.ge.p_low).and.(p.le.p_high)) goto 202
  if(p.gt.p_high) goto 302
202 q=p-0.5
  r=q*q
  z=(((((a1*r+a2)*r+a3)*r+a4)*r+a5)*r+a6)*q/ &
       &(((((b1*r+b2)*r+b3)*r+b4)*r+b5)*r+1)
  goto 204
302 if((p.gt.p_high).and.(p.lt.1)) goto 203
203 q=dsqrt(-2*dlog(1-p))
  z=-(((((c1*q+c2)*q+c3)*q+c4)*q+c5)*q+c6)/ &
       &((((d1*q+d2)*q+d3)*q+d4)*q+1)
204 dinvnorm=z
  return
end function dinvnorm
!####################



