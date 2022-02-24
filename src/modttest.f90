! ------------------------------------------------------------------------------
! This program computes a t-test for a Pearson correlation coefficient
! corrected for spatial autocorrelation, following Dutilleul (1993):
!
! Dutilleul, P. 1993. Modifying the t test for assessing the correlation 
!    between two spatial processes. Biometrics 49: 305-314.
!
! In this version of the program, the main input data file (text) must 
! contain 4 columns: Coordinate X, Coordinate Y, Variable 1, Variable 2.
! The program assumes the coordinates to be in a Cartesian plane.
!
! One may provide files with one's own limits of distance classes,
! pre-computed Moran's I autocorrelation coefficients, as well as
! an upper-triangular matrix of distance classes, if one so wishes.
! These elements are not necessary, however. In the absence of such
! matrices, the program will do all calculations from the input data matrix
! of the previous paragraph, using equidistant classes.
!
! If the user provides the value of the Spearman correlation coefficient,
! the associated probability will be computed and printed after that of
! the Pearson correlation coefficient.
!
! The probability computed by the program is for a two-tailed test.
! It is obtained by linear interpolation between the values computed
! for the integer values of d.f. above and below the corrected real-valued
! number of degrees of freedom, which is printed as "df" on output.
!
! Users of this program may refer to it in publications as follows:
!
! Legendre, P. 2000. Program Mod_t_test. Departement de sciences 
!    biologiques, Universite de Montreal. Available on the
!    WWWeb site <http://www.fas.umontreal.ca/BIOL/legendre/>
!
!                                             Pierre Legendre, June 2000
! 345678901234567890123456789012345678901234567890123456789012345678901234567890
!
! Modified by Paul Della-Marta and Andreas Phillip
!

!!$      subroutine modttest(nx,ny,lon,lat,data1,data2,PearR,df,prob)
!!$      integer :: nx,ny,x,y
!!$      real :: lat(ny),lon(nx),data1(nx*ny),data2(nx*ny)
!!$      Parameter (nmax=400, maxdist=nmax*(nmax-1)/2, maxclass=17)
!!$      ! The value of 'maxclass' is computed as follows: 1.5+3.3*log10(float(maxdist))
!!$      Integer dclass(maxdist),icard(maxclass)
!!$      Real*8  Data(nmax,4),vec(nmax),D(maxdist),PearR,SpearR,&
!!$     &        uppclass(maxclass),im(maxclass,2),df,F,Pof,pa,pb,prob
!!$      Real*8  A(nmax,nmax),Cor1(nmax,nmax),Cor2(nmax,nmax),&
!!$     &        Temp1(nmax,nmax),Temp2(nmax,nmax)
!!$      Character namea*79


      !subroutine modttest(nx,ny,nmax,lon,lat,data1,data2,PearR,df,prob)


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! i-n -> integer
      subroutine modttest(nx,ny,lon,lat,data1,data2,PearR,df,prob)
        implicit none
        integer :: nx,ny
        real(kind=8) :: lat(ny),lon(nx)
        real(kind=8) :: data1(nx*ny),data2(nx*ny)
        real(kind=8) :: PearR,df,prob
	integer :: nmax,maxdist,maxclass,nclass,ndist
	real(kind=8) :: dmaxdist,dmaxclass
        integer, allocatable :: dclass(:),icard(:)
	real(KIND=8), allocatable :: D(:),uppclass(:),im(:,:)
	integer :: i,x,y,n,nu2
        real(kind=8) :: Data(nx*ny,4),vec(nx*ny)
	integer :: ichoice1,ichoice2,ichoice3,ichoice4
        real(kind=8) :: A(nx*ny,nx*ny),Cor1(nx*ny,nx*ny),Cor2(nx*ny,nx*ny)
        real(kind=8) :: Temp1(nx*ny,nx*ny),Temp2(nx*ny,nx*ny)
        real(kind=8) :: F,Pof,pa,pb

        nmax=nx*ny
        n=nx*ny
        dmaxdist=nmax*(nmax-1)/2.d0
        maxdist=dmaxdist
        dmaxclass=1.5+3.3*log10(dmaxdist)
        maxclass=dmaxclass

!write(*,*)"nmax     =",nmax
!write(*,*)"dmaxdist  =",dmaxdist
!write(*,*)"maxclass =",maxclass

      allocate( dclass(maxdist), icard(maxclass) )
      allocate( D(maxdist), uppclass(maxclass),im(maxclass,2) )

         i=0
         do y=1,ny
            do x=1,nx
               i=i+1
               Data(i,1)=lon(x)
               Data(i,2)=lat(y)
               !write(*,"(3i4,4f10.2)")i,x,y,lon(x),lat(y),Data(i,1:2)
            enddo
         enddo
      Data(1:n,3)=data1
      Data(1:n,4)=data2

      nclass=0
      ndist=n*(n-1)/2
      ichoice1=0
      ichoice2=0
      ichoice3=0
      ichoice4=0


      if(nclass.eq.0) nclass=1.5+3.3*log10(float(ndist))


call EuclidD(n,nmax,Data,D,maxdist,nclass,maxclass,uppclass,ndist,&
     &             dclass,ichoice1)

do i=1,n
  vec(i)=Data(i,3)
enddo


call IMoran(n,nmax,vec,1,maxdist,nclass,maxclass,uppclass,&
     &               dclass,im)



do i=1,n
vec(i)=Data(i,4)
enddo

 call IMoran(n,nmax,vec,2,maxdist,nclass,maxclass,uppclass,&
     &               dclass,im)



     call Correl(n,nmax,Data,PearR)


      call DutTtest(n,nmax,dclass,maxdist,nclass,maxclass,icard,&
     &              im,df,A,Cor1,Cor2,Temp1,Temp2)



! Compute F and probability (with linear interpolation for real value of df)
      F=PearR*PearR*df/(1.0-PearR*PearR)
      nu2=df
      pa=Pof(F,1,nu2)
      pb=Pof(F,1,(nu2+1))
      prob=pa - (pa-pb)*(df-dfloat(nu2))


      end subroutine modttest
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      subroutine modttest1(nx,ny,lon,lat,data1,data2,PearR,df,prob)

      implicit none
      integer :: n,i,nclass,ndist,ichoice1,k,ichoice2,j,ichoice3,ichoice4,nu2
      real(kind=8) :: dmaxdist

      integer :: nx,ny,x,y
      real(kind=8) :: lat(ny),lon(nx),data1(nx*ny),data2(nx*ny)

      integer :: nmax,maxdist,maxclass

      integer, allocatable :: dclass(:),icard(:)
      real(KIND=8), allocatable :: D(:),uppclass(:),im(:,:)

      !Parameter (maxdist=(nx*ny)*((nx*ny)-1)/2, maxclass=17)
      ! The value of 'maxclass' is computed as follows: 1.5+3.3*log10(float(maxdist))

      !Integer dclass(maxdist),icard(maxclass)
      Real*8 Data(nx*ny,4),vec(nx*ny)
      real(kind=8) :: PearR,SpearR
      real(kind=8) df,F,Pof,pa,pb,prob


      Real*8  A(nx*ny,nx*ny),Cor1(nx*ny,nx*ny),Cor2(nx*ny,nx*ny)
      Real*8  Temp1(nx*ny,nx*ny),Temp2(nx*ny,nx*ny)
      Character namea*79


      write(*,*)"starting modttest ..." 
      read(*,*)

      nmax=nx*ny
      dmaxdist=nmax*(nmax-1)/2.d0
      maxdist=dmaxdist


write(*,*)"nmax     =",nmax
write(*,*)"dmaxdist  =",dmaxdist
write(*,*)"OK" ; stop


      !maxclass=1.5+3.3*log10(float(maxdist))
      maxclass=1.5+3.3*log10(dmaxdist)



write(*,*)"maxclass =",maxclass


      allocate( dclass(maxdist), icard(maxclass) )
      allocate( D(maxdist), uppclass(maxclass),im(maxclass,2) )
      


!
! Read parameters and data
!      write(*,110)
!      write(*,*) 'Name of input data file (containing 4 columns)'
!      read(*,*) namea
!      open (1,file=namea,Status='old')
!      inquire(unit=1,name=namea)
!      write(*,*) 'Data file: ',namea
!      write(*,*) 'Number of sampling units?'
!      read(*,*) n
       n=nx*ny
      if(n.gt.nmax) then
         write(*,*) 'n >',nmax
         write(*,*)&
     &        'Too many objects. Redimension and recompile the program.'
         stop
         endif
!      do 4 i=1,n
!    4 read(1,*) (Data(i,j), j=1,4)
!!$         do j=1,ny
!!$            Data( (j-1)*nx+1 : j*nx , 1)=lon(1:nx)
!!$            write(*,*)j, (j-1)*nx+1, j*nx
!!$         enddo
!!$         do j=1,nx
!!$            Data( (j-1)*ny+1 : j*ny , 2)=lat(1:ny)
!!$         enddo

         i=0
         do y=1,ny
            do x=1,nx
               i=i+1
               Data(i,1)=lon(x)
               Data(i,2)=lat(y)
               !write(*,"(3i4,4f10.2)")i,x,y,lon(x),lat(y),Data(i,1:2)
            enddo
         enddo

      Data(1:n,3)=data1
      Data(1:n,4)=data2
    
      nclass=0
      ndist=n*(n-1)/2
! Data file with distance classes provided?
      6 continue
      !write(*,*)
!      write(*,*)'Is there a file with upper limits of distance classes?'
!      write(*,*) '(0) No   (1) Yes'
!      read(*,*) ichoice1
      ichoice1=0
      if((ichoice1.lt.0).or.(ichoice1.gt.1)) goto 6
      if(ichoice1.eq.1) then
         write(*,*)'Name of file with upper limits of distance classes:'
         read(*,*) namea
         open (2,file=namea,Status='old')
         inquire(unit=2,name=namea)
         write(*,*)'File with upper limits of distance classes: ',namea
         write(*,*) 'How many classes?'
         read(*,*) nclass
         do 8 k=1,nclass
    !8    read(2,*) uppclass(k)
            read(2,*) uppclass(k)
8           continue
         endif
! Data file with Moran's I values provided?
   12 continue
         !write(*,*)
!      write(*,*) 
!     +       'Is there a file with Moran''s I coefficients? (2 columns)'
!      write(*,*) '(0) No   (1) Yes'
!      read(*,*) ichoice2
      ichoice2=0
      if((ichoice2.lt.0).or.(ichoice2.gt.1)) goto 12
      if(ichoice2.eq.1) then
         write(*,*) 'Name of file with Moran''s I coefficients:'
         read(*,*) namea
         open (3,file=namea,Status='old')
         inquire(unit=3,name=namea)
         write(*,*) 'File with Moran''s I coefficients: ',namea
         if(nclass.eq.0) then
            write(*,*) 'How many classes?'
            read(*,*) nclass
            endif
         do 14 k=1,nclass
   !14    read(3,*) (im(k,j), j=1,2)
       read(3,*) (im(k,j), j=1,2)
14          continue
         endif
!
! Data file with distance classes provided?
   16 continue
         !write(*,*)
!      write(*,*) 'Is there an UPPER TRIANGULAR file of distance classes'
!      write(*,*) '(without diagonal)?  --  (0) No   (1) Yes'
!      read(*,*) ichoice3
      ichoice3=0
      if((ichoice3.lt.0).or.(ichoice3.gt.1)) goto 16
      if(ichoice3.eq.1) then
         write(*,*) 'Name of file with distance classes:'
         read(*,*) namea
         open (4,file=namea,Status='old')
         inquire(unit=4,name=namea)
         write(*,*) 'File with distance classes: ',namea
         read(4,*) (dclass(i), i=1,ndist)
         endif
! Spearman correlation coefficient?
   20 continue
         !write(*,*)
!      write(*,*) 
!     +'The program will compute a Pearson correlation coefficient.'
!      write(*,*) 
!     +'Do you wish to provide a Spearman coefficient as well?'
!      write(*,*) '(0) No   (1) Yes'
!      read(*,*) ichoice4
      ichoice4=0
      if((ichoice4.lt.0).or.(ichoice4.gt.1)) goto 20
      if(ichoice4.eq.1) then
         write(*,*)& 
     &   'Type the Spearman correlation coefficient (Type -9 to cancel)'
         read(*,*) SpearR
         if(SpearR.lt.-1.0) ichoice4=0
      endif

! Start computing the corrected d.f.
      if(nclass.eq.0) nclass=1.5+3.3*log10(float(ndist))
!      temp=log10(float(ndist))
!      write(*,*) 'log10 =',temp,'   nclass =',nclass
      if(ichoice3.eq.0)&
     &call EuclidD(n,nmax,Data,D,maxdist,nclass,maxclass,uppclass,ndist,&
     &             dclass,ichoice1)
      if(ichoice2.eq.0) then
         do 50 i=1,n
   !50    vec(i)=Data(i,3)
            vec(i)=Data(i,3)
50          continue
         call IMoran(n,nmax,vec,1,maxdist,nclass,maxclass,uppclass,&
     &               dclass,im)
         do 52 i=1,n
   !52    vec(i)=Data(i,4)
            vec(i)=Data(i,4)
52          continue
         call IMoran(n,nmax,vec,2,maxdist,nclass,maxclass,uppclass,&
     &               dclass,im)
         endif
      call Correl(n,nmax,Data,PearR)
      call DutTtest(n,nmax,dclass,maxdist,nclass,maxclass,icard,&
     &              im,df,A,Cor1,Cor2,Temp1,Temp2)
! Compute F and probability (with linear interpolation for real value of df)
      F=PearR*PearR*df/(1.0-PearR*PearR)
      nu2=df
      pa=Pof(F,1,nu2)
      pb=Pof(F,1,(nu2+1))
      prob=pa - (pa-pb)*(df-dfloat(nu2))
! Print results: Correlograms and prob associated with Pearson r
!!$      write(*,101)
!!$      do 70 k=1,nclass
!!$   70 write(*,102) k,icard(k),uppclass(k),(im(k,j), j=1,2)
!!$      write(*,103) PearR,df,F,prob
! Spearman?
      if(ichoice4.eq.1) then
         F=SpearR*SpearR*df/(1.0-SpearR*SpearR)
         pa=Pof(F,1,nu2)
         pb=Pof(F,1,(nu2+1))
         prob=pa - (pa-pb)*(df-dfloat(nu2))
         write(*,104) SpearR,df,F,prob      
      endif
!
      if(ichoice1.eq.1) close(2)
      if(ichoice2.eq.1) close(3)
      if(ichoice3.eq.1) close(4)
      !stop
  100 format(8f10.5)
  101 format(/' Moran''s I table D_class   Card  UpperLimit  I(var.1)  I(var.2)'/)
  102 format(i5,i10,2x,3f10.5)
  !103 format(/' Pearson  r =',f8.5,'  df =',f10.5,'   F =',f10.5,'   Prob =',f8.5)
  103 format(' Pearson  r =',f8.5,'  df =',f10.5,'   F =',f10.5,'   Prob =',f8.5)
  !104 format(/' Spearman r =',f8.5,'  df =',f10.5,'   F =',f10.5,'   Prob =',f8.5)
  104 format(' Spearman r =',f8.5,'  df =',f10.5,'   F =',f10.5,'   Prob =',f8.5)
!!$  110 format(' t-test for a Pearson correlation coefficient corrected'/&
!!$     &       ' for spatial autocorrelation following Dutilleul (1993)'//&
!!$     &       '                              (c) Pierre Legendre, 2000'/&
!!$     &       '                    Departement de sciences biologiques'/&
!!$     &       '                                 Universite de Montreal'/)
      end
      
      
!----------------------------------------------------------------
Subroutine Correl(n,nmax,Data,r)
  Real*8 Data(nmax,4)
  Real*8 sx,sx2,sy,sy2,sxy,varx,vary,covxy,r,x,y
  dfln=dfloat(n)
  sx=0.0
  sx2=0.0
  sy=0.0
  sy2=0.0
  sxy=0.0
  do i=1,n
     x=Data(i,3)
     y=Data(i,4)
     sx=sx+x
     sx2=sx2+x*x
     sy=sy+y
     sy2=sy2+y*y
      sxy=sxy+x*y
   enddo
   ! The values of 'varx', 'vary' and 'covxy' are not divided by
   ! 'dfloat(n-1)', since this is not required to compute r.
   ! They are simple sums of squared deviations from the means (SSC).
   varx=sx2-sx*sx/dfln
   vary=sy2-sy*sy/dfln
   covxy=sxy-sx*sy/dfln
   r=covxy/sqrt(varx*vary)
   return
end Subroutine Correl
      

!----------------------------------------------------------------
Subroutine DutTtest(n,nmax,dclass,maxdist,nclass,maxclass,icard,&
      & im,df,A,Cor1,Cor2,Temp1,Temp2)

implicit none
   integer :: n,i,ii,nclass,ipos,itemp,nmax,maxdist,maxclass

   Integer dclass(maxdist),icard(maxclass)
   Real*8 im(maxclass,2),A(nmax,nmax),Cor1(nmax,nmax),&
        &       Cor2(nmax,nmax),Temp1(nmax,nmax),Temp2(nmax,nmax),&
        &       invn,trace1,trace2,trace3,df
!
! Source code from Pierre Dutilleul ((c)2000) in SAS IML
! translated to Fortran and adapted by Pierre Legendre ((c)2000).
!
! The input variables are the following:
! 
! n: number of observations.
! im: two-column matrix. Each column contains Moran's I statistics
!     for the successive distance classes, for one of the variables.
! dclass: vector representing an unfolded upper-triangular matrix of 
!     distance classes for the various pairs of objects.
! r: Pearson correlation calculated between the two variables.
!
! There are only 2 variables in this version of the program.
!
! Initialization




   invn=1.0/dfloat(n)


   do i=1,n
      do ii=1,n
         A(i,ii)=0.0-invn
         Cor1(i,ii)=0.0
         Cor2(i,ii)=0.0
      enddo
      A(i,i)=1.0-invn
      Cor1(i,i)=1.0
      Cor2(i,i)=1.0
   enddo

!write(*,*)"still running ..."
!return

   !do k=1,nclass
   !   icard(k)=0
   !enddo

   icard(1:nclass)=0

!write(*,*)"still running ..."
!return

! Fill the two square matrices of correlations among objects

      ipos=0
      do 10 i=1,n
         do 8 ii=i+1,n
            ipos=ipos+1
            itemp=dclass(ipos)
            if(itemp.eq.0) goto 8
            icard(itemp)=icard(itemp)+1
!            write(*,120) ipos,itemp
!  120       format(2i5)
            Cor1(i,ii)=im(itemp,1)
            Cor1(ii,i)=Cor1(i,ii)
    8       continue
   10    continue

      ipos=0
      do 20 i=1,n
         do 18 ii=i+1,n
            ipos=ipos+1
            itemp=dclass(ipos)
            if(itemp.eq.0) goto 18
            Cor2(i,ii)=im(itemp,2)
            Cor2(ii,i)=Cor2(i,ii)
   18       continue
   20    continue

! Compute the corrected number of degrees of freedom

      Call Product(A,Cor1,Temp1,n,n,n,nmax,nmax,nmax)
      trace1=0.0
      do 22 i=1,n
   !22 trace1=trace1+Temp1(i,i)
         trace1=trace1+Temp1(i,i)
22       continue

      Call Product(A,Cor2,Temp2,n,n,n,nmax,nmax,nmax)
      trace2=0.0
      do 24 i=1,n
   !24 trace2=trace2+Temp2(i,i)
         trace2=trace2+Temp2(i,i)
24       continue

      Call Product(Temp1,Temp2,A,n,n,n,nmax,nmax,nmax)
      trace3=0.0
      do 26 i=1,n
   !26 trace3=trace3+A(i,i)
         trace3=trace3+A(i,i)
26       continue

! Calculate the corrected number of degrees of freedom

      df=(trace1*trace2/trace3) - 1.0
!
      return
end


!----------------------------------------------------------------
Subroutine EuclidD(n,nmax,Data,D,maxdist,nclass,maxclass,&
     &                   uppclass,ndist,dclass,ichoice1)
      Integer dclass(maxdist)
      Real*8 Data(nmax,4),D(maxdist),dmax,uppclass(maxclass),&
     &       classinterval,temp,epsilon
! This subroutine computes Euclidean distances among sampling units
      epsilon=0.0000000001
      dmax=0.0
      ipos=0
      do 11 i=1,n
      do 10 ii=i+1,n
      ipos=ipos+1
      temp=0.0
      do 4 j=1,2
    !4 temp=temp+(Data(i,j)-Data(ii,j))**2
         temp=temp+(Data(i,j)-Data(ii,j))**2
4        continue
      temp=dsqrt(temp)
      D(ipos)=temp
      if(temp.gt.dmax) dmax=temp
   10 continue
11    continue
!      write(*,*) 'Dmax =',dmax
      if(ichoice1.eq.1) goto 22
! ... as well as the upper limit of each distance class 
!     (equal frequency classes), if not provided by the user
      classinterval=dmax/dfloat(nclass)
!      write(*,*) 'classinterval =',classinterval
      temp=0
      do 20 k=1,nclass
      temp=temp+classinterval
      uppclass(k)=temp
!      write(*,*) 'uppclass(',k,')',uppclass(k)
   20 continue
! Write a half-matrix of distance classes (integers) in 'dclass'
   22 do 24 k=1,nclass
   !24 uppclass(k)=uppclass(k)+epsilon
         uppclass(k)=uppclass(k)+epsilon
24       continue
      do 30 i=1,ndist
      dclass(i)=0
      temp=D(i)
      do 28 k=1,nclass
      if(temp.le.uppclass(k)) then
         dclass(i)=k
         goto 30
         endif
   28 continue
!   30 write(*,120) i,temp,dclass(i)
   30 continue
      return
  120 format(i5,f20.15,i5)
      end


!----------------------------------------------------------------
Subroutine IMoran(n,nmax,y,icol,maxdist,nclass,maxclass,uppclass,&
     &                  dclass,im)
      Integer dclass(maxdist)
      Real*8 y(nmax)
      Real*8 uppclass(maxclass),im(maxclass,2), &
     &       dfln,temp,w,sy,sy2,ybar,var
      integer h ! modified
!
      dfln=dfloat(n)
      sy=0.0
      sy2=0.0
      do 2 i=1,n
      sy=sy+y(i)
      sy2=sy2+y(i)*y(i)
    2 continue
      ybar=sy/dfln
      var=(sy2-(sy**2)/dfln)/dfln
!
      do 50 k=1,nclass
      temp=0.0
      w=0.0
      ipos=0
      do 41 h=1,n ! modified 40 -> 41
      do 40 i=h+1,n
      ipos=ipos+1
      if(dclass(ipos).ne.k) goto 40
         w=w+1.0
         temp=temp+((y(h)-ybar)*(y(i)-ybar))
   40 continue
41       continue ! modified 40 -> 41
      im(k,icol)=(temp/w)/var
   50 continue
end
      

!----------------------------------------------------------------
Function Pof(F,df1,df2)
! ALGORITHM Compute probability of F ratio.
!	Adapted from Collected Algorithms of the CACM
!	Algorithm 322
!	Egon Dorrer
      Integer df1,df2,a,b,i,j
      Real*8  I_PI,pof,F,w, y, z, d, p, F_EPSILON
      F_EPSILON=0.000001
      I_PI=0.3183098861837906715377675	
!
      if((F.lt.F_EPSILON).or.(df1.lt.1).or.(df2.lt.1)) then
         pof=1.0
         return
		 endif
      if(mod(df1,2).gt.0) then
         a=1
         else
         a=2
         endif
      if(mod(df2,2).gt.0) then
         b=1
         else
         b=2
         endif
      w = (F * dfloat(df1)) / dfloat(df2)
      z = 1.0 / (1.0 + w)
	     if (a.eq.1) then
	        if (b.eq.1) then
               p = dsqrt (w)
               y = I_PI
               d = y * z / p
               p = 2.0 * y * datan(p)
            else
               p = dsqrt (w * z)
               d = 0.5 * p * z / w
            endif
	     else if (b.eq.1) then
            p = dsqrt (z)
            d = 0.5 * z * p
            p = 1.0 - p
	     else
            d = z * z
            p = w * z
         endif
	     y = 2.0 * w / z
      do 10 j = (b+2), df2, 2
      d = d * ((1.0 + dfloat(a) / dfloat(j-2)) * z)
      if(a.eq.1) then
         p = p + d * y / dfloat(j-1)
         else
         p = (p + w) * z
         endif
   10 continue
      y = w * z
      z = 2.0 / z
      b = df2 - 2
      do 20 i = (a+2), df1, 2
      j = i + b
      d = d * (y * dfloat(j) / dfloat(i-2))
      p = p - (z * d / dfloat(j))
   20 continue
! Correction for approximation errors suggested in certification 
      if (p.lt.0.0) then
            p = 0.0
         else if (p.gt.1.0) then
            p = 1.0
         endif
      pof=1.0-p
!
      return
end

!----------------------------------------------------------------
Subroutine Product(Z1,Z2,Z3,n1,n2,n3,n1max,n2max,n3max)
      Real*8 Z1(n1max,n2max),Z2(n2max,n3max),Z3(n1max,n3max),temp
! Z1 * Z2 = Z3
      do 9 i=1,n1
      do 8 j=1,n3
      temp=0.0
      do 6 k=1,n2
    !6 temp=temp+Z1(i,k)*Z2(k,j)
         temp=temp+Z1(i,k)*Z2(k,j)
6        continue
    !8 Z3(i,j)=temp
         Z3(i,j)=temp
8        continue
9        continue
      return
end

