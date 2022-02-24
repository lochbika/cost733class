!
! iguapca
!
! Run Principal Component Analysis (with orthogonal rotation if desired) 
! using svd in double precision.
! Modified routines from netlib (svd) and linpack (matrix inversion) are used. 
! Compile with any fortran90 compiler.
! Copyright (C) 2008 Andreas Philipp, Institute for Geography, University of Augsburg
!
!    This program is free software: you can redistribute it and/or modify
!    it under the terms of the GNU General Public License as published by
!    the Free Software Foundation, either version 3 of the License, or
!    (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!    GNU General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see <http://www.gnu.org/licenses/>.
!
! last modified 22.05.2008
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine pca(nobs,nvar, &
     & dat, &
     & norm,npc,exvarsum,rot, &
     & loadings,scores,exvar)
  implicit none
  integer :: nobs,nvar
  real(kind=8) :: dat(nvar,nobs)
  real(kind=8) :: scores(nobs,nvar) ! scores(nobs,npc)
  real(kind=8) :: exvar(nvar) ! exvar(npc)
  real(kind=8) :: loadings(nvar,nvar) ! loadings(nvar,npc)
  !real(kind=8),allocatable :: a(:,:)
  real(kind=8) :: a(nobs,nvar)
  real(kind=8),allocatable :: ldg(:,:)
  real(kind=8),allocatable :: sco(:,:)
  integer :: norm ! 0 = raw, 1 = centered (covariance), 2 = normalized
  integer :: npc  ! number of pcs
  integer :: allpcs
  real(kind=8) :: exvarsum ! if >0.D0 selects the number of PCs
  integer :: rot ! 
  integer :: var,pc,obs

  real(kind=8) :: sdev(nvar),mean
  real(kind=8) :: totalvariance
  logical :: cov=.true.
  logical :: mirror=.true.

  real(kind=8) :: gam=0.D0
  real(kind=8) :: Targ(nobs,nvar)
  integer :: W(nobs,nvar)
  real(kind=8) :: kappa=0.D0

  logical :: kaisernorm=.true.
  logical :: gpanorm=.true.

  W=0
  Targ=0.D0
  allpcs=nvar

  ! provide data for pca
  !write(*,*)"data ..."
  !allocate(a(nobs,nvar))
  do obs=1,nobs
     a(obs,1:nvar)=dat(1:nvar,obs)
  enddo

  ! normalization
  !write(*,*)"norm ..."
  totalvariance=0.D0
  do var=1,nvar
     ! normalize
     mean=sum(a(1:nobs,var))/nobs
     sdev(var)=sqrt( sum( ( a(1:nobs,var) - mean )**2 ) / (nobs-1) )
     select case (norm)
     case(0)
        totalvariance=totalvariance+sdev(var)**2
        cov=.true.
     case(1)
        a(1:nobs,var)=(a(1:nobs,var)-mean)
        totalvariance=totalvariance+sdev(var)**2
        cov=.true.
     case(2)
        a(1:nobs,var)=(a(1:nobs,var)-mean)/sdev(var)
        totalvariance=totalvariance+1.D0
        cov=.false.
     end select
  enddo

  ! pca -> retrieve all PCs for selection
  !write(*,*)"svd ...",totalvariance
  call svdpca(nobs,nvar,a,totalvariance,cov,nvar,loadings,scores,exvar)

  !write(*,*)"exvar:",exvarsum
  !write(*,"(10f10.4)")exvar(1:nvar)

  ! select
  if(exvarsum>0.D0)then
     npc=allpcs
     do pc=1,allpcs
        !write(*,"(1i4,2f10.4)")pc,exvar(pc),sum(exvar(1:pc))
        if(sum(exvar(1:pc))>exvarsum)then
           npc=pc
           exit
        endif
     enddo
  endif

  !write(*,*)"rotation ...",npc,rot

  allocate(ldg(nvar,npc),sco(nobs,npc))

  ldg(1:nvar,1:npc)=loadings(1:nvar,1:npc)
  sco(1:nobs,1:npc)=scores(1:nobs,1:npc)

  ! rotation
  if(rot>=0.and.rot<=2)then
     call rotate(nobs,nvar,a(1:nobs,1:nvar),sdev(1:nvar),totalvariance,  &
          &  cov,rot,kaisernorm,npc,ldg(1:nvar,1:npc),sco(1:nobs,1:npc),exvar(1:npc))
  endif
  if(rot>=10.and.rot<=26)then
     call gpa(nobs,nvar,npc,a(:,:),ldg(:,1:npc),sco(1:nobs,1:npc),  &
          &  exvar(1:npc),sdev(1:nvar),totalvariance,gpanorm,cov,rot, &
          & gam,Targ(1:nobs,1:npc),W(1:nobs,1:npc),kappa)
  endif
  
  !write(*,*)"mirror ..."
  ! mirror
  if(mirror)then
     do pc=1,npc
        var=maxloc(dabs(ldg(:,pc)),1)
        if(ldg(var,pc)<0.D0)then
           ldg(:,pc)=-1.D0*ldg(:,pc)
           sco(:,pc)=-1.D0*sco(:,pc)
        endif
     enddo
  endif

  !write(*,*)"copy ..."
  loadings=0.D0
  loadings(1:nvar,1:npc)=ldg(1:nvar,1:npc)
  scores=0.D0
  scores(1:nobs,1:npc)=sco(1:nobs,1:npc)

  !write(*,*)"done ..."
  return

end subroutine pca

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine svdpca(nobs,nvar,a,totalvariance, &
     & cov,npc,loadings,scores,exvar)
  implicit none
  integer :: nobs,nvar,npc
  integer :: var,pc !obs,
  real(kind=8) :: loadings(nvar,npc),scores(nobs,npc),exvar(npc),totalvariance

  integer :: i,j,k,l,m,n,i1,k1,l1,nm,iteration,niteration,ierr !,ii,kk,ll,mn
  !double precision a(nm,n),w(n),u(nm,n),v(nm,n),rv1(n)
  !real(kind=8) :: a(m,n),w(n),u(nm,n),v(nm,n),rv1(n)
  real(kind=8) :: a(nobs,nvar)
  real(kind=4),allocatable :: w(:),u(:,:),rv1(:)
  real(kind=4),allocatable :: v(:,:) ! will not be allocated since matv=.false.
  real(kind=4) :: c,f,g,h,s,x,y,z,scale,anorm
  !real(kind=8) :: dsqrt,dmax1,dabs,dsign
  logical matu,matv,cov
  real(kind=8) :: mem,mb=4.0/1024.D0/1024.D0
  integer :: verbose=1

  ! note: cov is not necessary here! Isn't used!
  !if(verbose>3)write(*,*)"cov =",cov

  !write(*,*)"starting svdpca ..."

  niteration=5000
  matu=.true.
  matv=.false. ! loadings will be calculated from w and u only for npc

  m=nobs
  n=nvar
  nm=max(n,m)

  mem=n*mb
  mem=mem+nm*n*mb
  mem=mem+n*mb
  call verbosity(verbose)
  if(verbose>2.and.mem>1024)then
     write(*,*)"cov =",cov
     write(*,"(a,1f14.6,a)")" additional memory consumption for SVD =",mem," Mb"
  endif

  allocate(w(n),u(nm,n),rv1(n))

  !     http://www.netlib.org/fmm/svd.f
  !     this subroutine is a translation of the algol procedure svd,
  !     num. math. 14, 403-420(1970) by golub and reinsch.
  !     handbook for auto. comp., vol ii-linear algebra, 134-151(1971).
  !     this subroutine determines the singular value decomposition
  !          t
  !     a=usv  of a real m by n rectangular matrix.  householder
  !     bidiagonalization and a variant of the qr algorithm are used.
  !     on input:
  !        nm must be set to the row dimension of two-dimensional
  !          array parameters as declared in the calling program
  !          dimension statement.  note that nm must be at least
  !          as large as the maximum of m and n.
  !        m is the number of rows of a (and u).
  !        n is the number of columns of a (and u) and the order of v.
  !        a contains the rectangular input matrix to be decomposed.
  !        matu should be set to .true. if the u matrix in the
  !          decomposition is desired, and to .false. otherwise.
  !        matv should be set to .true. if the v matrix in the
  !          decomposition is desired, and to .false. otherwise.
  !     on output:
  !        a is unaltered (unless overwritten by u or v).
  !        w contains the n (non-negative) singular values of a (the
  !          diagonal elements of s).  they are unordered.  if an
  !          error exit is made, the singular values should be correct
  !          for indices ierr+1,ierr+2,...,n.
  !        u contains the matrix u (orthogonal column vectors) of the
  !          decomposition if matu has been set to .true.  otherwise
  !          u is used as a temporary array.  u may coincide with a.
  !          if an error exit is made, the columns of u corresponding
  !          to indices of correct singular values should be correct.
  !        v contains the matrix v (orthogonal) of the decomposition if
  !          matv has been set to .true.  otherwise v is not referenced.
  !          v may also coincide with a if u is not needed.  if an error
  !          exit is made, the columns of v corresponding to indices of
  !          correct singular values should be correct.
  !        ierr is set to
  !          zero       for normal return,
  !          k          if the k-th singular value has not been
  !                     determined after 30 iterations.
  !        rv1 is a temporary storage array.
  !     this is a modified version of a routine from the eispack
  !     collection by the nats project
  !     modified to eliminate machep
  !     modified to fortran90 format by andreas.philipp@geo.uni-augsburg.de

  ierr = 0
  
  ! copy data into U
  do k=1,n
     u(1:m,k)=a(1:m,k)
  enddo
  
  ! householder reduction to bidiagonal form 
  !write(*,*)"householder reduction to bidiagonal form ..."
  g = 0.0
  scale = 0.0
  anorm = 0.0
  
  do i = 1, n
     l = i + 1
     rv1(i) = scale * g
     g = 0.0
     s = 0.0
     scale = 0.0
     
     if(i<=m)then
        do k = i, m
           scale = scale + abs(u(k,i))
        enddo
        if (scale /= 0.0)then
           do k = i, m
              u(k,i) = u(k,i) / scale
              s = s + u(k,i)**2
           enddo
           f = u(i,i)
           g = -sign(sqrt(s),f)
           h = f * g - s
           u(i,i) = f - g
           do  j = l, n
              s = 0.0
              do k = i, m
                 s = s + u(k,i) * u(k,j)
              enddo
              f = s / h
              do  k = i, m
                 u(k,j) = u(k,j) + f * u(k,i)
              enddo
           enddo
           do k = i, m
              u(k,i) = scale * u(k,i)
           enddo
        endif
     endif

     w(i) = scale * g
     g = 0.0
     s = 0.0
     scale = 0.0
     if( i<=m .and. i/=n )then
        do k = l, n
           scale = scale + abs(u(i,k))
        enddo
        if(scale.ne.0.0)then
           do k = l, n
              u(i,k) = u(i,k) / scale
              s = s + u(i,k)**2
           enddo
           f = u(i,l)
           g = -sign(sqrt(s),f)
           h = f * g - s
           u(i,l) = f - g
           do k = l, n
              rv1(k) = u(i,k) / h
           enddo
           do j = l, m
              s = 0.0
              do k = l, n
                 s = s + u(j,k) * u(i,k)
              enddo
              do k = l, n
                 u(j,k) = u(j,k) + s * rv1(k)
              enddo
           enddo
           do k = l, n
              u(i,k) = scale * u(i,k)
           enddo
        endif
     endif

  enddo
  anorm=maxval(abs(w)+abs(rv1))
  
  ! accumulation of right-hand transformations
  if (matv) then
     !write(*,*)"accumulation of right-hand transformations (v) ..."
     allocate(v(nm,n))
     do i = n, 1, -1 ! for i=n step -1 until 1 do --

        if(i.lt.n)then
           if(g.ne.0.0)then
              do j = l, n ! double division avoids possible underflow
                 v(j,i) = (u(i,j) / u(i,l)) / g
              enddo
              do j = l, n
                 s = 0.0
                 do k = l, n
                    s = s + u(i,k) * v(k,j)
                 enddo
                 do k = l, n
                    v(k,j) = v(k,j) + s * v(k,i)
                 enddo
              enddo
           endif
           do j = l, n
              v(i,j) = 0.0
              v(j,i) = 0.0
           enddo
        endif

        v(i,i) = 1.0
        g = rv1(i)
        l = i
     enddo
  endif !matv

  ! accumulation of left-hand transformations
  !write(*,*)"accumulation of left-hand transformations (u) ..."
  if ( matu) then
     do i=min(m,n),1,-1 ! for i=min(m,n) step -1 until 1 do --
        l = i + 1
        g = w(i)
        u(i,l:n)=0.0
        if (g /= 0.0) then
           do j = l, n
              s = SUM( u(l:m,i) * u(l:m,j) )
              f = (s / u(i,i)) / g ! double division avoids possible underflow
              u(i:m,j)=u(i:m,j)+f* u(i:m,i)
           enddo
           u(i:m,i) = u(i:m,i) / g
        else
           u(i:m,i)=0.0
        endif
        u(i,i) = u(i,i) + 1.0
     enddo
  endif
    
  ! diagonalization of the bidiagonal form 
  !write(*,*)"diagonalization of the bidiagonal form ..."
  do k=n,1,-1 ! for k=n step -1 until 1 do --
     k1=k-1
     
     do iteration = 1, niteration        
        
        ! test for splitting.
        do l=k,1,-1 ! for l=k step -1 until 1 do --
           l1=l-1
           
           ! rv1(1) is always zero, so there is no exit
           ! through the bottom of the loop
           if (abs(rv1(l)) + anorm .eq. anorm) exit
           if (abs(w(l1)) + anorm .eq. anorm) then
              ! cancellation of rv1(l) if l greater than 1
              c = 0.0
              s = 1.0              
              do i = l, k
                 f = s * rv1(i)
                 rv1(i) = c * rv1(i)
                 if (abs(f) + anorm .eq. anorm)exit
                 g = w(i)
                 h = sqrt(f*f+g*g)
                 w(i) = h

                 c = g / h
                 s = -f / h

                 if (matu)then
                    do j = 1, m
                       y = u(j,l1)
                       z = u(j,i)
                       u(j,l1) = y * c + z * s
                       u(j,i) = -y * s + z * c
                    enddo
                 endif

              enddo
              
              exit
           endif
        enddo
        
        ! test for convergence
        z = w(k)
        if (l .eq. k) then !go to 650
           ! convergence
           if (z < 0.0) then
              w(k) = -z
              if(matv)v(1:n,k)=-v(1:n,k)
           endif
           exit 
        endif
        
        ! no convergence
        if (iteration .eq. niteration)then
           write(*,*)"no convergence in SVD!"
           stop
           ierr = k
           return
        endif
        
        ! shift from bottom 2 by 2 minor
        x = w(l)
        y = w(k1)
        g = rv1(k1)
        h = rv1(k)
        f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y)
        g = sqrt(f*f+1.0)
        f = ((x - z) * (x + z) + h * (y / (f + sign(g,f)) - h)) / x
        
        ! next qr transformation
        c = 1.0
        s = 1.0
        do i1 = l, k1
           i = i1 + 1
           g = rv1(i)
           y = w(i)
           h = s * g
           g = c * g
           z = sqrt(f*f+h*h)
           rv1(i1) = z
           c = f / z
           s = h / z
           f = x * c + g * s
           g = -x * s + g * c
           h = y * s
           y = y * c
           if (matv) then
              do j = 1, n
                 x = v(j,i1)
                 z = v(j,i)
                 v(j,i1) = x * c + z * s
                 v(j,i) = -x * s + z * c
              enddo
           endif
           
           z = sqrt(f*f+h*h)
           w(i1) = z ! rotation can be arbitrary if z is zero
           if (z .ne. 0.0) then
              c = f / z
              s = h / z
           endif
           f = c * g + s * y
           x = -s * g + c * y
           
           if(matu)then
              do j = 1, m
                 y = u(j,i1)
                 z = u(j,i)
                 u(j,i1) = y * c + z * s
                 u(j,i) = -y * s + z * c
              enddo
           endif
           
        enddo
        rv1(l) = 0.0
        rv1(k) = f
        w(k) = x
               
     enddo ! iterations
  enddo ! k

  ! -----------------------------------------------------
  ! transform SVD results into PCA results

  ! scores
  do pc=1,npc
     scores(1:nobs,pc)=u(1:nobs,pc)*w(pc)
  enddo

  ! eigenvalues
  w(1:nvar)=w(1:nvar)**2/(nobs)

  ! scaling of scores
  do pc=1,npc
     if(w(pc)/=0.D0)scores(1:nobs,pc)=scores(1:nobs,pc)/sqrt(w(pc))
  enddo

  !write(*,*)"unrotated scores:"
  !do obs=1,nobs
  !   write(*,"(10f10.6)")scores(obs,1:npc)
  !enddo

  ! calculate loadings
  do pc=1,npc
     do var=1,nvar
        loadings(var,pc)=SUM( a(1:nobs,var)*scores(1:nobs,pc) ) /nobs
     enddo
  enddo

  !write(*,*)"unrotated loadings:"
  !do var=1,nvar
  !   write(*,"(10f10.6)")loadings(var,1:npc)
  !enddo

  ! explained variance
  do pc=1,npc
     exvar(pc)=w(pc)/totalvariance
  enddo

  !write(*,*)"explained variance for unrotated PCs:"
  !do pc=1,npc
  !   write(*,"(a,1i4,1f20.10)")"pc ",pc,exvar(pc)
  !enddo

end subroutine svdpca

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine rotate(nobs,nvar,a,sdev,totalvariance,cov,rot,norm,npc, &
     & loadings,scores,exvar)

  implicit none
  integer :: nobs,nvar,npc
  integer :: obs,var,pc
  real(kind=8) :: loadings(nvar,npc),scores(nobs,npc),exvar(npc)
  real(kind=8) :: totalvariance,sdev(nvar)

  integer :: iteration,niteration
  real(kind=8) :: a(nobs,nvar)
  !real(kind=8) :: mem,mb=8.D0/1024.D0/1024.D0

  ! rotation
  real(kind=8), allocatable :: pctransform(:,:),rowsdev(:)
  real(kind=8) :: omega ! rotation method
  real(kind=8) :: small,alpha,beta,gamma,delta,phi,cosphi,sinphi,wks
  real(kind=8) :: x,y,exvarmax
  integer :: pc1,pc2,rot,i
  logical :: cov,norm ! kaiser-normalisation
  logical :: goon
  integer :: rank(npc),pcnum(npc)
  real(kind=8), allocatable :: loadingsT(:,:),loadingsM(:,:),coeffs(:,:)
  real(kind=8), allocatable :: tmpload(:,:),tmpscor(:,:), tmpexvar(:)

  niteration=500

  !mem=npc*npc
  !if(norm)mem=mem+nvar
  !mem=mem+npc*nvar
  !mem=mem+2*npc**2

  ! decide rotation method
  omega=0.D0
  select case (rot)
  case (-1) ! no rotation
     return
  case (0) ! quartimax: each variable loads high on one/a few pcs
     omega=0.D0/nvar
  case (1) ! varimax: all pcs have a few high loadings 
     omega=1.D0/nvar
  case (2) ! eqamax: compromise between quartimax and varimax
     omega=(npc/2.D0)/nvar
  end select

  ! transfomation matrix
  allocate(pctransform(npc,npc)) 
  pctransform=0.D0
  do pc=1,npc
     pctransform(pc,pc)=1.D0
  enddo

  ! prepare rotated loading matrix
  if(norm)then
     allocate(rowsdev(nvar))
     do var=1,nvar ! kaiser(row) normalisation
        rowsdev(var)=SQRT(SUM(loadings(var,1:npc)**2))
        !write(*,"(a,1f20.10)")"rowsdev:",rowsdev(var)
        loadings(var,1:npc)=loadings(var,1:npc)/rowsdev(var)
     enddo
  endif

!!$  write(*,*)"normalized loadings:"
!!$  do var=1,nvar
!!$     write(*,"(10f10.6)")loadings(var,1:npc)
!!$  enddo

  ! begin rotation iterations
  small=0.0001 
  do iteration=1,niteration
     goon=.false.

     do pc=1,npc-1
        do pc1=pc+1,npc

           ! angle phi
           alpha=0.D0
           beta=0.D0
           gamma=0.D0
           delta=0.D0
           do var=1,nvar
              x = (loadings(var,pc)+loadings(var,pc1)) * (loadings(var,pc)-loadings(var,pc1))
              y = 2.0*loadings(var,pc)*loadings(var,pc1)
              alpha=alpha+x
              beta=beta+y
              gamma=gamma+(x+y)*(x-y)
              delta=delta+x*y
           enddo
           delta=2.D0*delta
           delta=delta-2.D0*alpha*beta*omega
           gamma=gamma-(alpha+beta)*(alpha-beta)*omega
           phi=0.25D0*atan2(delta,gamma)

           ! check for angle beeing too small, if not: rotate
           if(abs(phi)>small*0.25D0)then
              cosphi=cos(phi)
              sinphi=sin(phi)
              ! rotate matrices
              do var=1,nvar
                 wks = loadings(var,pc)*cosphi + loadings(var,pc1)*sinphi
                 loadings(var,pc1) = -loadings(var,pc)*sinphi + loadings(var,pc1)*cosphi
                 loadings(var,pc) = wks
              enddo
              do pc2=1,npc
                 wks = pctransform(pc2,pc)*cosphi + pctransform(pc2,pc1)*sinphi
                 pctransform(pc2,pc1) = -pctransform(pc2,pc)*sinphi + pctransform(pc2,pc1)*cosphi
                 pctransform(pc2,pc) = wks
              enddo
              goon=.true.
           endif

        enddo
     enddo
     if(goon)cycle ! if there was a rotation for any of the pc-pairs: go on
     exit ! else exit
  enddo ! iterations

!!$  write(*,*)"rotated loadings:"
!!$  do var=1,nvar
!!$     write(*,"(10f10.6)")loadings(var,1:npc)
!!$  enddo

  ! denormalise loadings
  if(norm)then
     do var=1,nvar
        loadings(var,1:npc)=loadings(var,1:npc)*rowsdev(var)
     enddo
     deallocate(rowsdev)
  endif

!!$  write(*,*)"denormalized loadings:"
!!$  do var=1,nvar
!!$     write(*,"(10f10.6)")loadings(var,1:npc)
!!$  enddo

  ! -----------------------------------------------------
  ! recalculate scores (least squares method): scores = a � ( loadings � (loadingsT � loadings)^-1 )

  ! transpose loadings matrix
  allocate(loadingsT(npc,nvar))
  do pc=1,npc
     do var=1,nvar
        loadingsT(pc,var)=loadings(var,pc)
     enddo
  enddo

  ! multiply with loadings matrix
  allocate(loadingsM(npc,npc))
  do pc1=1,npc
     do pc2=1,npc
        loadingsM(pc1,pc2)=SUM(loadingsT(pc1,1:nvar)*loadings(1:nvar,pc2))
     enddo
  enddo

  deallocate(loadingsT)

!!$  write(*,*)"multiplied with transposed loadings matrix:"
!!$  do pc1=1,npc
!!$     write(*,"(10f10.6)")loadingsM(1:npc,pc1)
!!$  enddo

  ! inverse multiplied loadings matrix
  !write(*,*)"inverse ..."
  call dgedi(loadingsM,npc,npc)

!!$  write(*,*)"inverse matrix:"
!!$  do pc1=1,npc
!!$     write(*,"(10f10.6)")loadingsM(1:npc,pc1)
!!$  enddo

  ! multiply with loadings matrix
  allocate(coeffs(nvar,npc))
  do var=1,nvar
     do pc=1,npc
        coeffs(var,pc)=SUM(loadingsM(pc,1:npc)*loadings(var,1:npc))
     enddo
  enddo
  deallocate(loadingsM)

!!$  write(*,*)"loadings matrix multiplied with inverse:"
!!$  do var=1,nvar
!!$     write(*,"(10f10.6)")coeffs(var,1:npc)
!!$  enddo

  ! multiply dat with coeff matrix = scores
  do obs=1,nobs
     do pc=1,npc
        scores(obs,pc)=SUM(a(obs,1:nvar)*coeffs(1:nvar,pc))
     enddo
  enddo
  deallocate(coeffs)

  ! calculate explained variance
  !write(*,"(1f20.10)")totalvariance
  !write(*,"(21f10.4)")sdev(1:nvar)
  if(cov)then
     do pc=1,npc
        !exvar(pc)=0.D0
        !do var=1,nvar
        !   exvar(pc)=exvar(pc)+((loadings(var,pc)/sdev(var))**2)*(sdev(var)**2)
        !enddo
        !exvar(pc)=exvar(pc)/totalvariance
        exvar(pc)=SUM( ((loadings(1:nvar,pc)/sdev(1:nvar))**2)*(sdev(1:nvar)**2) ) /totalvariance
        !write(*,"(21f10.4)")exvar(pc)
     enddo
  else
     do pc=1,npc
        exvar(pc)=SUM(loadings(1:nvar,pc)**2)/totalvariance
     enddo
  endif
 
  ! sort again, since exvar has changes
  rank(1:npc)=0
  do i=1,npc
     ! find PC with maximum exvar that has not yet beeing ranked
     exvarmax=-9.D0
     do pc=1,npc
        if(exvar(pc)>exvarmax.and.rank(pc)==0)then
           pc1=pc
           exvarmax=exvar(pc)
        endif
     enddo
     ! give it the rank, save pc for that rank
     rank(pc1)=i
     pcnum(i)=pc1
  enddo
  ! rearrange exvar, loadings and scores
  allocate(tmpexvar(npc),tmpload(nvar,npc),tmpscor(nobs,npc))
  do i=1,npc
     tmpexvar(i)=exvar(pcnum(i))
     tmpload(1:nvar,i)=loadings(1:nvar,pcnum(i))
     tmpscor(1:nobs,i)=scores(1:nobs,pcnum(i))
  enddo
  exvar=tmpexvar
  loadings=tmpload
  scores=tmpscor
  deallocate(tmpexvar,tmpload,tmpscor)

end subroutine rotate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dgefa ( a, lda, n, ipvt, info )

  ! DGEFA factors a real general matrix.
  !    FORTRAN90 translation by John Burkardt. 07 March 2001
  !  Reference:
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !  Parameters:
  !    Input/output, real ( kind = 8 ) A(LDA,N).
  !    On intput, the matrix to be factored.
  !    On output, an upper triangular matrix and the multipliers used to obtain
  !    it.  The factorization can be written A=L*U, where L is a product of
  !    permutation and unit lower triangular matrices, and U is upper triangular.
  !
  !    Input, integer LDA, the leading dimension of A.
  !
  !    Input, integer N, the order of the matrix A.
  !
  !    Output, integer IPVT(N), the pivot indices.
  !
  !    Output, integer INFO, singularity indicator.
  !    0, normal value.
  !    K, if U(K,K) == 0.  This is not an error condition for this subroutine,
  !    but it does indicate that DGESL or DGEDI will divide by zero if called.
  !    Use RCOND in DGECO for a reliable indication of singularity.

  implicit none
  
  integer :: lda, n
  real(kind=8) :: a(lda,n),t
  integer :: info,ipvt(n),idamax,j,k,l

  !  Gaussian elimination with partial pivoting.
  info = 0
  do k = 1, n - 1
     !  Find L = pivot index.
     l = idamax ( n-k+1, a(k,k), 1 ) + k - 1
     ipvt(k) = l
     !  Zero pivot implies this column already triangularized.
     if ( a(l,k) == 0.0D+00 ) then
        info = k
        cycle
     endif
     !  Interchange if necessary.
     if ( l /= k ) then
        t = a(l,k)
        a(l,k) = a(k,k)
        a(k,k) = t
     endif
     !  Compute multipliers.
     t = -1.0D+00 / a(k,k)
     call dscal ( n-k, t, a(k+1,k), 1 )
     !  Row elimination with column indexing.
     do j = k+1, n
        t = a(l,j)
        if ( l /= k ) then
           a(l,j) = a(k,j)
           a(k,j) = t
        end if
        call daxpy ( n-k, t, a(k+1,k), 1, a(k+1,j), 1 )
     enddo
  enddo

  ipvt(n) = n
  if ( a(n,n) == 0.0D+00 ) then
    info = n
  endif

  return
end subroutine dgefa

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
INTEGER FUNCTION IDAMAX(N,DX,INCX)

  !     finds the index of element having max. absolute value.
  !     jack dongarra, linpack, 3/11/78.
  !     modified 3/93 to return if incx .le. 0.
  !     modified 12/3/93, array(1) declarations changed to array(*)

  INTEGER :: INCX,N
  DOUBLE PRECISION DX(*)

  DOUBLE PRECISION DMAX
  INTEGER I,IX
  !     .. Intrinsic Functions ..
  INTRINSIC DABS

  IDAMAX = 0
  IF (N.LT.1 .OR. INCX.LE.0) RETURN
  IDAMAX = 1
  IF (N.EQ.1) RETURN
  IF (INCX.EQ.1) GO TO 20

  ! code for increment not equal to 1
  IX = 1
  DMAX = DABS(DX(1))
  IX = IX + INCX
  DO 10 I = 2,N
     IF (DABS(DX(IX)).LE.DMAX) GO TO 5
     IDAMAX = I
     DMAX = DABS(DX(IX))
5    IX = IX + INCX
10   CONTINUE
     RETURN
     ! code for increment equal to 1
20   DMAX = DABS(DX(1))
     DO 30 I = 2,N
        IF (DABS(DX(I)).LE.DMAX) GO TO 30
        IDAMAX = I
        DMAX = DABS(DX(I))
30   CONTINUE
     RETURN
END FUNCTION IDAMAX

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine dgedi ( a, lda, n )
  !subroutine dgedi ( a, lda, n, ipvt, det, work, job )
  !
  ! DGEDI computes the determinant and inverse of a matrix factored by DGECO or DGEFA.
  !  Discussion:
  !    A division by zero will occur if the input factor contains
  !    a zero on the diagonal and the inverse is requested.
  !    It will not occur if the subroutines are called correctly
  !    and if DGECO has set 0.0 < RCOND or DGEFA has set INFO == 0.
  !    
  !  Reference:
  !    Jack Dongarra, Jim Bunch, Cleve Moler, Pete Stewart,
  !    LINPACK User's Guide,
  !    SIAM, 1979,
  !    ISBN13: 978-0-898711-72-1,
  !    LC: QA214.L56.
  !    FORTRAN90 translation by John Burkardt. 17 May 2005
  !
  !  Parameters:
  !    Input/output, real ( kind = 8 ) A(LDA,N), on input, the  LU factor
  !       information, as output by DGECO or DGEFA.  On output, the inverse
  !       matrix, if requested.
  !    Input, integer LDA, the leading dimension of the array A.
  !    Input, integer N, the order of the matrix A.
  !    Input, integer IPVT(N), the pivot vector from DGECO or DGEFA.
  !    Workspace, real ( kind = 8 ) WORK(N).
  !    Output, real ( kind = 8 ) DET(2), the determinant of original matrix if
  !      requested.  The determinant = DET(1) * 10.0**DET(2)
  !      with  1.0D+00 <= abs ( DET(1) ) < 10.0D+00
  !      or DET(1) == 0.0D+00.
  !    Input, integer JOB, specifies what is to be computed.
  !      11, both determinant and inverse.
  !      01, inverse only.
  !      10, determinant only.

  implicit none
  integer :: lda,n

  real(kind=8) :: a(lda,n),det(2)
  integer :: i,ipvt(n),j,job,k,l
  real(kind=8) :: t,work(n)
  integer :: info
  integer :: verbose

  job=01

  call dgefa ( a, lda, n, ipvt, info )

  call verbosity(verbose)
  if(verbose>2.and.info/=0)then
     write(*,*)"info from dgefa /= 0!"
  endif

  ! Compute the determinant.
  if ( job / 10 /= 0 ) then
     !write(*,*)"determinant ..."
     det(1) = 1.0D+00
     det(2) = 0.0D+00
     do i = 1, n
        if ( ipvt(i) /= i ) then
           det(1) = -det(1)
        end if
        det(1) = det(1) * a(i,i)        
        if ( det(1) == 0.0D+00 ) then
           exit
        end if
        do while ( abs ( det(1) ) < 1.0D+00 ) 
           det(1) = det(1) * 10.0D+00
           det(2) = det(2) - 1.0D+00
        end do
        do while ( 10.0D+00 <= abs ( det(1) ) )
           det(1) = det(1) / 10.0D+00
           det(2) = det(2) + 1.0D+00
        end do
    end do
  end if

  ! Compute inverse(U).
  if ( mod ( job, 10 ) /= 0 ) then
     do k = 1, n
        a(k,k) = 1.0D+00 / a(k,k)
        t = -a(k,k)
        call dscal ( k-1, t, a(1,k), 1 )
        do j = k+1, n
           t = a(k,j)
           a(k,j) = 0.0D+00
           call daxpy ( k, t, a(1,k), 1, a(1,j), 1 )
        end do
     end do
     !  Form inverse(U) * inverse(L).
     do k = n-1, 1, -1
        work(k+1:n) = a(k+1:n,k)
        a(k+1:n,k) = 0.0D+00
        do j = k+1, n
           t = work(j)
           call daxpy ( n, t, a(1,j), 1, a(1,k), 1 )
        end do
        l = ipvt(k)
        if ( l /= k ) then
           call dswap ( n, a(1,k), 1, a(1,l), 1 )
        end if
     end do
  end if
  return
end subroutine dgedi

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DSCAL(N,DA,DX,INCX)
  !     scales a vector by a constant.
  !     uses unrolled loops for increment equal to one.
  !     jack dongarra, linpack, 3/11/78.
  !     modified 3/93 to return if incx .le. 0.
  !     modified 12/3/93, array(1) declarations changed to array(*)
  
  DOUBLE PRECISION DA
  INTEGER INCX,N
  DOUBLE PRECISION DX(*)
  INTEGER I,M,MP1,NINCX
  INTRINSIC MOD

  IF (N.LE.0 .OR. INCX.LE.0) RETURN
  IF (INCX.EQ.1) GO TO 20

  ! code for increment not equal to 1
  NINCX = N*INCX
  DO I = 1,NINCX,INCX
     DX(I) = DA*DX(I)
  enddo
  RETURN

  ! code for increment equal to 1
  ! clean-up loop
20 continue

  M = MOD(N,5)
  IF (M.EQ.0) GO TO 40
  DO I = 1,M
     DX(I) = DA*DX(I)
  enddo
  IF (N.LT.5) RETURN
40 MP1 = M + 1
  DO I = MP1,N,5
     DX(I) = DA*DX(I)
     DX(I+1) = DA*DX(I+1)
     DX(I+2) = DA*DX(I+2)
     DX(I+3) = DA*DX(I+3)
     DX(I+4) = DA*DX(I+4)
  enddo
  RETURN
END SUBROUTINE DSCAL

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DSWAP(N,DX,INCX,DY,INCY)
  !     interchanges two vectors.
  !     uses unrolled loops for increments equal one.
  !     jack dongarra, linpack, 3/11/78.
  !     modified 12/3/93, array(1) declarations changed to array(*)

  INTEGER INCX,INCY,N
  DOUBLE PRECISION DX(*),DY(*)
  DOUBLE PRECISION DTEMP
  INTEGER I,IX,IY,M,MP1
  INTRINSIC MOD
  
  IF (N.LE.0) RETURN
  IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
  ! code for unequal increments or equal increments not equal to 1
  IX = 1
  IY = 1
  IF (INCX.LT.0) IX = (-N+1)*INCX + 1
  IF (INCY.LT.0) IY = (-N+1)*INCY + 1
  DO I = 1,N
     DTEMP = DX(IX)
     DX(IX) = DY(IY)
     DY(IY) = DTEMP
     IX = IX + INCX
     IY = IY + INCY
  enddo
  RETURN

  ! code for both increments equal to 1
  ! clean-up loop
20 continue 
  M = MOD(N,3)
  IF (M.EQ.0) GO TO 40
  DO I = 1,M
     DTEMP = DX(I)
     DX(I) = DY(I)
     DY(I) = DTEMP
  enddo
  IF (N.LT.3) RETURN
40 continue
  MP1 = M + 1
  DO I = MP1,N,3
     DTEMP = DX(I)
     DX(I) = DY(I)
     DY(I) = DTEMP
     DTEMP = DX(I+1)
     DX(I+1) = DY(I+1)
     DY(I+1) = DTEMP
     DTEMP = DX(I+2)
     DX(I+2) = DY(I+2)
     DY(I+2) = DTEMP
  enddo
  RETURN
END SUBROUTINE DSWAP

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
  
  !     constant times a vector plus a vector.
  !     uses unrolled loops for increments equal to one.
  !     jack dongarra, linpack, 3/11/78.
  !     modified 12/3/93, array(1) declarations changed to array(*)

  DOUBLE PRECISION DA
  INTEGER INCX,INCY,N
  DOUBLE PRECISION DX(*),DY(*)
  INTEGER I,IX,IY,M,MP1
  INTRINSIC MOD
  
  IF (N.LE.0) RETURN
  IF (DA.EQ.0.0d0) RETURN
  IF (INCX.EQ.1 .AND. INCY.EQ.1) GO TO 20
  
  ! code for unequal increments or equal increments
  ! not equal to 1
  IX = 1
  IY = 1
  IF (INCX.LT.0) IX = (-N+1)*INCX + 1
  IF (INCY.LT.0) IY = (-N+1)*INCY + 1
  DO I = 1,N
     DY(IY) = DY(IY) + DA*DX(IX)
     IX = IX + INCX
     IY = IY + INCY
  enddo
  RETURN

  ! code for both increments equal to 1
  ! clean-up loop
20 continue
  M = MOD(N,4)
  IF (M.EQ.0) GO TO 40
  DO I = 1,M
     DY(I) = DY(I) + DA*DX(I)
  enddo
  IF (N.LT.4) RETURN
40 continue
  MP1 = M + 1
  DO I = MP1,N,4
     DY(I) = DY(I) + DA*DX(I)
     DY(I+1) = DY(I+1) + DA*DX(I+1)
     DY(I+2) = DY(I+2) + DA*DX(I+2)
     DY(I+3) = DY(I+3) + DA*DX(I+3)
  enddo
  RETURN
END SUBROUTINE DAXPY

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine gpa(o,m,n,Dat,L,S,exvar,sdev,totvar,norm,cov,criti,gam,Targ,W,kappa)
  !
  ! oblique only
  !
  !  Source of Gradient Projektion Algorithm (GPA):
  ! Coen A. Bernaards and Robert I. Jennrich
  ! Gradient Projection Algorithms and Software for Arbitrary Rotation Criteria in Factor Analysis
  ! Educational and Psychological Measurement 2005; 65; 676; DOI: 10.1177/0013164404272507
  ! The online version of this article can be found at: http://epm.sagepub.com/cgi/content/abstract/65/5/676
  !
  !  Source of rotation criteria and gradients:
  ! Website: http://www.stat.ucla.edu/research
  !
  ! FORTRAN90 adaptation by Florian Streicher 10.2009
  !
  ! INPUT: - L(m,n) (loading matrix)
  !        additionally: - gam (gamma for oblimin)
  !                      - Targ(m,n) (target matrix for target rotation)
  !                      - W(m,n) (for partially specified target rotation)
  !                      - kappa (for Crawford-Ferguson)
  ! OUTPUT: - L(m,n) (rotated loadings)
  !
  implicit none
  integer::o,m,n
  integer::iter1,iter2 !i,
  real(8)::al,sc,v(n),f,ft
  real(8),dimension(o,m)::Dat  ! initial Data
  real(8),dimension(o,n)::S  ! scores
  real(8),dimension(m,n)::A,L,Gq
  real(8),dimension(n,n)::T,Ti,Tt,G,Gp,X
  ! l,m,n: input matrix dimensions
  ! L: input matrix, current rotation, and finally the result
  ! T,Ti,Tt: rotation matrix, inverse, and transposed
  ! G,Gp,Gq: gradient matrix, gradient projection, and current gradient
  real(8)::exvar(n),sdev(m),totvar  ! explained variance, standard deviation, total variance
  logical::norm,cov  ! normalize, covarriance matrix
  integer::criti  ! rotation criteria
  character(25)::crit  ! rotation criteria
  real(8)::gam,kappa  ! oblimin gamma, kappa for crawford-ferguson
  real(8),dimension(m,n)::Targ  ! target matrix
  integer,dimension(m,n)::W  ! for partially specified Target

  ! IDENTITY MATRIX
  !data (T(i,i),i=1,n)/n*1.D0/
  T=makediag(n,n,1.D0,0.D0)

  A=L

  call crita(criti,crit)

  if(norm)call norma(m,n,A,A,"nr")

  ! GRADIENT PROJECTION ALGORITHM / OBLIQUE ROTATION
  al=1.D0
  Ti=T
  call dgedi(Ti,n,n)
  L=matmul(A,transpose(Ti))
  call vgf(m,n,L,f,Gq,crit,gam,Targ,W,kappa)
  G=-transpose(matmul(transpose(L),matmul(Gq,Ti)))
  do iter1=0,500
     Gp=G-matmul(T,vectdiag(n,n,n,sum(T*G,1),0.D0))
     !s=sqrt(trace(n,n,matmul(transpose(Gp),Gp)))  ! Frobenius norm
     sc=sqrt(sum(Gp**2))  ! Frobenius norm, also
     if(sc<0.00001)exit
     !write(*,"(1i3,2x,3f10.4)")iter1,f,log10(s),al  ! iter f log10(s) al
     al=2.D0*al
     do iter2=0,10
        X=T-al*Gp
        v=1.D0/sqrt(sum(X**2,1))
        Tt=matmul(X,vectdiag(n,n,n,v,0.D0))
        Ti=Tt
        call dgedi(Ti,n,n)
        L=matmul(A,transpose(Ti))
        call vgf(m,n,L,ft,Gq,crit,gam,Targ,W,kappa)
        if(ft<f-0.5*sc**2*al)exit
        al=al/2.D0
     enddo
     T=Tt
     f=ft
     G=-transpose(matmul(transpose(L),matmul(Gq,Ti)))
  enddo
  !Th=T
  !Lh=L
  !Phi=matmul(transpose(T),T)
  !write(*,"(3f12.7)")transpose(L)

  if(norm)call norma(m,n,L,A,"dr")

  call recalc(o,m,n,Dat,L,S,exvar,sdev,totvar,cov)

contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine vgf(m,n,L,f,Gq,crit,gam,Targ,W,kappa)
  ! ROTATION CRITERIA
  implicit none
  character(25)::crit
  integer::m,n
  real(8)::f
  real(8),dimension(m,n)::L,L2,Gq
  ! oblimin,oblimax
  real(8)::gam
  real(8),dimension(m,n)::L4,X
  ! target,pst
  real(8),dimension(m,n)::Targ,Btilde
  integer,dimension(m,n)::W
  ! simplimax
  integer,dimension(m,n)::I
  real(8),dimension(m,n)::L2up
  ! bentler
  real(8),dimension(n,n)::M1,D
  ! geomin
  real(8),dimension(m)::pro
  ! crawford-ferguson
  real(8)::kappa
  integer,dimension(m,m)::Mo
  integer,dimension(n,n)::No
  ! infomax
  real(8),dimension(m,n)::S,E,H,G0,G1,G2,Omn
  real(8)::s0,q0,q1,q2
  real(8),dimension(m)::s1,h1,e1
  real(8),dimension(n)::s2,h2,e2

  ! CRITERIA
  select case (trim(crit))

    case ("oblimin","quartimin","biquartimin","covarmin")  ! oblimin family
       if(crit=="oblimin")gam=gam
       if(crit=="quartimin")gam=0.D0  ! direct oblimin
       if(crit=="biquartimin")gam=0.5D0
       if(crit=="covarmin")gam=1.D0
       L2=L**2
       X=matmul(L2,makediag(n,n,0.D0,1.D0))
       if(gam/=0.D0)X=matmul((makediag(m,m,1.D0-gam/m,0.D0-gam/m)),X)
       f=sum(L2*X)/4
       Gq=L*X

    case ("oblimax")
       L2=L**2
       L4=L**4
       Gq=-(4*L**3/sum(L4)-4*L/sum(L2))
       f=-(log(sum(L4))-2*log(sum(L2)))

    case ("target")
       Gq=2.D0*(L-Targ)
       f=sum((L-Targ)**2)

    case ("pst")  ! partially specified target
       Btilde=W*Targ
       Gq=2*(W*L-Btilde)
       f=sum((W*L-Btilde)**2)

    case ("simplimax")
       L2=L**2
       L2up=L2
       I=0
       where(L2<=mth(m*n,L2up,m))I=1
       Gq=2.D0*I*L
       f=sum(I*L2)

    case ("bentler")  ! invariant pattern simplicity
       L2=L**2
       M1=matmul(transpose(L2),L2)
       D=vectdiag(n,n,n,readdiag(n,n,M1),0.D0)
       call dgedi(M1,n,n)
       call dgedi(D,n,n)
       Gq=-L*(matmul(L2,M1-D))
       f=-(log(det(n,M1))-log(det(n,D)))/4

    case ("geomin")
       L2=L**2+0.01D0
       pro=exp(sum(log(L2),2)/n)
       Gq=2.D0/n*L/L2*spread(pro,2,n)
       f=sum(pro)

    case ("cf","quartimax","varimax","equamax","parsimax","parsimony")  ! Crawford-Ferguson family
       if(crit=="cf")kappa=kappa
       if(crit=="quartimax")kappa=0.D0
       if(crit=="varimax")kappa=1.D0/m
       if(crit=="equamax")kappa=n/2.D0/m
       if(crit=="parsimax")kappa=(n-1.D0)/(m+n-2.D0)
       if(crit=="parsimony")kappa=1.D0  ! factor parsimony...
       Mo=makediag(m,m,0.D0,1.D0)
       No=makediag(n,n,0.D0,1.D0)
       L2=L**2
       Gq=(1.D0-kappa)*L*(matmul(L2,No))+kappa*L*matmul(Mo,L2)
       f=(1.D0-kappa)*sum(readdiag(n,n,matmul(transpose(L2),matmul(L2,No))))/4  &
            &  +kappa*sum(readdiag(n,n,matmul(transpose(L2),matmul(Mo,L2))))/4

    case ("infomax")  ! McKeon
       S=L**2
       s0=sum(S)
       s1=sum(S,2)
       s2=sum(S,1)
       E=S/s0
       e1=s1/s0
       e2=s2/s0
       q0=sum(-E*log(E))
       q1=sum(-e1*log(e1))
       q2=sum(-e2*log(e2))
       f=log(1.D0*n)+q0-q1-q2
       H=-(log(E)+1)
       Omn=makediag(m,n,1.D0,1.D0)
       G0=H/s0-sum(S*H)/s0**2*Omn
       h1=-(log(e1)+1)
       G1=spread(h1,2,n)/s0-sum(s1*h1/s0**2)*Omn
       h2=-(log(e2)+1)
       G2=spread(h2,1,m)/s0-sum(h2*s2/s0**2)*Omn
       Gq=2.D0*L*(G0-G1-G2)

    !case ("tandem1")
    !case ("tandem2")  ! Comrey
    !case ("entropy")  ! minumum entropy
    !case ("mccammon")  ! McCammon minimum entropy ratio
    case default
       write(*,*)"ERROR: no rotation criteria!";stop
  end select
end subroutine vgf

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine crita(criti,crit)
  implicit none
  integer::criti
  character(25)::crit
  if(criti==10)crit="oblimin"
  if(criti==11)crit="quartimin"
  if(criti==12)crit="biquartimin"
  if(criti==13)crit="covarmin"
  if(criti==14)crit="oblimax"
  if(criti==15)crit="simplimax"
  if(criti==16)crit="infomax"
  if(criti==17)crit="geomin"
  if(criti==18)crit="target"
  if(criti==19)crit="pst"
  if(criti==20)crit="cf"
  if(criti==21)crit="quartimax"
  if(criti==22)crit="varimax"
  if(criti==23)crit="equamax"
  if(criti==24)crit="parsimax"
  if(criti==25)crit="parsimony"
  if(criti==26)crit="bentler"
end subroutine crita

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine norma(m,n,A,D,nor)
  ! (de)normalise rows  !/cols
  implicit none
  character(2)::nor
  integer::m,n,i
  real(8)::A(m,n),D(m,n),rowsdev(m)
  ! dim dev
  if(nor=="nr".or.nor=="dr")then
     do i=1,m
        rowsdev(i)=SQRT(SUM(D(i,:)**2))
     enddo
  endif
  ! normalise
  if(nor=="nr")then
     do i=1,m ! kaiser(row) normalisation
        A(i,:)=A(i,:)/rowsdev(i)
     enddo
  endif
  ! denormalise
  if(nor=="dr")then
     do i=1,m
        A(i,:)=A(i,:)*rowsdev(i)
     enddo
  endif
end subroutine norma

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine recalc(l,m,n,A,loadings,scores,exvar,sdev,totvar,cov)
  ! recalculate scores (least squares method): scores = a � ( loadings � (loadingsT � loadings)^-1 )
  implicit none
  integer:: l,m,n  ! nobs,nvar,npc
  integer:: pc,pci=0,i
  real(8):: A(l,m)
  real(8):: loadings(m,n),scores(l,n),exvar(n),lli(n,n)
  real(8):: tmpload(m,n),tmpscor(l,n),tmpexvar(n)
  real(8):: sdev(m),totvar,exvarmax
  integer:: rank(n),pcnum(n)
  logical:: cov

  ! scores = a � ( loadings � (loadingsT � loadings)^-1 )
  lli=matmul(transpose(loadings),loadings)
  call dgedi(lli,n,n)
  scores=matmul(A,matmul(loadings,lli))

  ! calculate explained variance
  if(cov)then
     do pc=1,n
        exvar(pc)=SUM(((loadings(:,pc)/sdev(:))**2)*(sdev(:)**2))/totvar
     enddo
  else
     do pc=1,n
        exvar(pc)=SUM(loadings(:,pc)**2)/totvar
     enddo
  endif

  ! sort again, since exvar has changes
  rank=0
  do i=1,n
     ! find PC with maximum exvar that has not yet been ranked
     exvarmax=-9.D0
     do pc=1,n
        if(exvar(pc)>exvarmax.and.rank(pc)==0)then
           pci=pc
           exvarmax=exvar(pc)
        endif
     enddo
     ! give it the rank, save pc for that rank
     rank(pci)=i
     pcnum(i)=pci
  enddo
  ! rearrange exvar, loadings and scores
  do i=1,n
     tmpexvar(i)=exvar(pcnum(i))
     tmpload(:,i)=loadings(:,pcnum(i))
     tmpscor(:,i)=scores(:,pcnum(i))
  enddo
  exvar=tmpexvar
  loadings=tmpload
  scores=tmpscor

end subroutine recalc

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function matmult(l,m,n,A,B)
  ! MATRIX MULTIPLICATION
  implicit none
  integer::l,m,n,i,j
  real(8),dimension(l,n)::matmult,C
  real(8),dimension(l,m)::A
  real(8),dimension(m,l)::At
  real(8),dimension(m,n)::B
  At=trans(l,m,A)
  do j=1,n
     do i=1,l
        C(i,j)=sum(At(:,i)*B(:,j))
     enddo
  enddo
  matmult=C
end function matmult

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
recursive function det(n,A) result(res)
  ! MATRIX DETERMINANT
  implicit none
  integer::n,i,i1,i2,j,j1,j2,k,rowz(n),colz(n)
  real(8)::res
  real(8),dimension(n,n)::A
  logical,dimension(n,n)::rowcol
  real(8),dimension(n)::Dn
  ! SELECT FOR SIZE OF MATRIX
  res=0.D0
  if(n==1)res=A(1,1)
  if(n==2)res=A(1,1)*A(2,2)-A(1,2)*A(2,1)
  ! Sarrus
  if(n==3)res=A(1,1)*A(2,2)*A(3,3)+A(1,2)*A(2,3)*A(3,1)+A(1,3)*A(2,1)*A(3,2) &
           & -A(3,1)*A(2,2)*A(1,3)-A(3,2)*A(2,3)*A(1,1)-A(3,3)*A(2,1)*A(1,2)
  ! Laplace
  if(n>=4)then
     ! ROW/COL WITH MAX OF ZEROS
     colz=count(A==0.D0,1)
     rowz=count(A==0.D0,2)
     if(maxloc((/colz,rowz/),1)>n)then
        i1=maxloc(rowz,1)
        i2=i1
        j1=1
        j2=n
     else
        i1=1
        i2=n
        j1=maxloc(colz,1)
        j2=j1
     endif
     ! (RECURSIVE) DETERMINANT
     k=0
     do j=j1,j2
        do i=i1,i2
           k=k+1
           if(A(i,j)==0.D0)then
              Dn(k)=0.D0
              cycle
           endif
           rowcol=.true.
           rowcol(i,:)=.false.
           rowcol(:,j)=.false.
           Dn(k)=A(i,j)*det(n-1,pack(A,rowcol))
        enddo
     enddo
     do k=2,n,2
        Dn(k)=-Dn(k)
     enddo
     res=sum(Dn)
  endif
end function det

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function trans(m,n,A)
  ! TRANSPOSE MATRIX
  implicit none
  integer::m,n,i
  real(8),dimension(n,m)::trans,At
  real(8),dimension(m,n)::A
  do i=1,n
     At(i,:)=A(:,i)
  enddo
  trans=At
end function trans

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function inv(n,A)
  ! INVERSE MATRIX
  ! needs: adj,det,trans
  implicit none
  integer::n
  real(8),dimension(n,n)::inv,A
  inv=trans(n,n,adj(n,A))/det(n,A)
end function inv

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function adj(n,A)
  ! ADJUNCT MATRIX
  ! needs: det
  implicit none
  integer::n,i,j,c
  real(8),dimension(n,n)::adj,Ad,A
  logical,dimension(n,n)::rowcol
  c=-1
  do j=1,n
     do i=1,n
        c=-c
        rowcol=.true.
        rowcol(i,:)=.false.
        rowcol(:,j)=.false.
        Ad(i,j)=c*det(n-1,pack(A,rowcol))
     enddo
  enddo
  adj=Ad
end function adj

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function trace(m,n,A)
  ! MATRIX DIAGONAL SUM
  implicit none
  integer::m,n,i
  real(8)::trace,s
  real(8),dimension(m,n)::A
  s=0.D0
  do i=1,min(m,n)
     s=s+A(i,i)
  enddo
  trace=s
end function trace

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function readdiag(m,n,A)
  ! MATRIX DIAGONAL TO VECTOR
  implicit none
  integer::m,n,i
  real(8),dimension(m+n)::readdiag,v
  !real(8),allocatable::readdiag(:),v(:)
  real(8),dimension(m,n)::A
  !allocate(readdiag(min(m,n)),v(min(m,n)))
  forall(i=1:min(m,n))v(i)=A(i,i)
  readdiag=v
end function readdiag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function makediag(m,n,x,y)
  ! MATRIX WITH X ON DIAGONAL AND Y ELSEWHERE
  implicit none
  integer::m,n,i
  real(8),dimension(m,n)::makediag,D
  real(8)::x,y
  D=y
  forall(i=1:min(m,n))D(i,i)=x
  makediag=D
end function makediag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function vectdiag(m,n,l,x,y)
  ! MATRIX WITH X(:) ON DIAGONAL AND Y ELSEWHERE
  implicit none
  integer::m,n,l,i
  real(8),dimension(m,n)::vectdiag,D
  real(8)::x(l),y
  D=y
  if(l<min(m,n))stop
  forall(i=1:min(m,n))D(i,i)=x(i)
  vectdiag=D
end function vectdiag

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
function mth(n,A,m)
  ! MTH ELEMENT OF A(N) BY SIZE
  implicit none
  integer::m,n,i,j
  real(8)::mth
  real(8)::A(n),b
  do j=2,n
     b=A(j)
     do i=j-1,1,-1
        if(A(i)<b)exit
        A(i+1)=A(i)
     enddo
     A(i+1)=b
  enddo
  mth=A(m)
end function mth

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end subroutine gpa
