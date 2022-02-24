!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sortcla()
  ! sort the CLA vector by class size
  use globvar
  implicit none
  integer :: obs,cl,sizemax,i,clmax
  integer :: clsize(NCL)
  integer :: rank(NCL),clatmp(NOBS)
  clmax=-1
  clsize=0
  do obs=1,NOBS
     clsize(CLA(obs))=clsize(CLA(obs))+1
  enddo
  rank(1:NCL)=0
  do i=1,NCL
     sizemax=0
     do cl=1,NCL
        if(clsize(cl)>sizemax.and.rank(cl)==0)then
           clmax=cl
           sizemax=clsize(cl)
        endif
     enddo
     rank(clmax)=i
     !clnum(i)=clmax
  enddo
  clatmp=CLA
  do obs=1,NOBS
     CLA(obs)=rank(clatmp(obs))
  enddo
end subroutine sortcla

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sortcla4dat(sdat)
  ! sort the CLA vector by class size
  use globvar
  implicit none
  integer :: obs,cl,i,clmax
  !integer :: clsize(NCL)
  real(kind=8) :: sdat(NCL),rdat(NCL),sizemax
  integer :: rank(NCL),clatmp(NOBS)
  clmax=-1
  !clsize=0
  !do obs=1,NOBS
  !   clsize(CLA(obs))=clsize(CLA(obs))+1
  !enddo
  rank(1:NCL)=0
  do i=1,NCL
     sizemax=minval(sdat)-999
     do cl=1,NCL
        !if(clsize(cl)>sizemax.and.rank(cl)==0)then
        if(sdat(cl)>sizemax.and.rank(cl)==0)then
           clmax=cl
           sizemax=sdat(cl)
        endif
     enddo
     rank(clmax)=i
     !clnum(i)=clmax
  enddo
  clatmp=CLA
  do obs=1,NOBS
     if(clatmp(obs)==0)then
        CLA(obs)=0
        cycle
     endif
     if(clatmp(obs)==-1)then
        CLA(obs)=-1
        cycle
     endif
     CLA(obs)=rank(clatmp(obs))
  enddo

  do cl=1,NCL
     rdat(rank(cl))=sdat(cl)
  enddo
  sdat=rdat

end subroutine sortcla4dat


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine sortcla4mdat(no,nv,nc,clas,sdat)
  ! sort the CLA vector by class size
  implicit none
  integer :: no,nv,nc
  integer :: clas(no)
  real(kind=8) :: sdat(nv,nc),sdattmp(nv,nc)
  real(kind=8) :: cnt(nc)
  integer :: o,c,cl,clmax
  integer :: rank(nc)
  integer :: clatmp(no)
  real(kind=8) :: valmax

  ! mean sdat for each class
  do c=1,nc
     cnt(c)=sum(sdat(1:nv,c))
  enddo

  ! sort
  clmax=-1
  rank(1:nc)=0
  do c=1,nc
     valmax=minval(cnt)-999
     do cl=1,nc
        if(cnt(cl)>valmax.and.rank(cl)==0)then
           clmax=cl
           valmax=cnt(cl)
        endif
     enddo
     rank(clmax)=c
  enddo
  clatmp=clas

  ! change
  do o=1,no
     if(clatmp(o)<1)then ! keep missing values
        clas(o)=clatmp(o)
        cycle
     endif
     clas(o)=rank(clatmp(o))
  enddo

  ! copy data
  do cl=1,nc
     sdattmp(1:nv,rank(cl))=sdat(1:nv,cl)
  enddo
  sdat=sdattmp

end subroutine sortcla4mdat
