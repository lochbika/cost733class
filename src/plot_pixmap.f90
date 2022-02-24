!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine pixmap(nx,ny,dat,sig,mv,xlen,ylen,xlabel,ylabel,  title,ofile)
  implicit none
  character*(*) :: ofile,title
  integer :: nx,ny,x,y
  real(kind=8) :: dat(nx,ny)
  integer :: sig(nx,ny)
  real(kind=8) :: xlen,ylen
  real(kind=8) mv,minv,maxv,range,cstep
  real(kind=8) :: r,g,b
  integer :: ytic,xtic,i
  character(len=20) :: xlabel(nx),ylabel(ny)

  integer :: nylegend,yllegend
  real(kind=8) :: lstep,xx,yy
  real(kind=8) :: legwidth,legheight,legstep
  real(kind=8) :: pageurx,pageury,pagellx,pagelly
  integer :: irange,iend
  character(len=20) :: colscheme
  logical :: shownumbers=.false.
  logical :: showlegend=.true. !.false.
  logical :: showtitle=.false.
  logical :: showsigsymbols=.true.
  colscheme="grey"


  ! UPPER RIGHT COORDINATES FOR BOUNDINGBOX (=PAGE)
  pagellx=-1*xlen
  pagelly=-1.3*xlen
  pageurx=(nx+1)*xlen
  pageury=(ny+1)*ylen

  if(showtitle.or.showlegend)then
     pagellx=-2*xlen
     pagelly=-2*xlen
     pageurx=(nx+6)*xlen
     pageury=(ny+3)*ylen
  endif

  xtic=1
  ytic=1

  maxv=MAXVAL(dat,dat/=mv)
  !minv=MINVAL(dat,dat/=mv)
  !maxv=+1.D0
  minv=-1.D0
  minv=0.D0
  range=maxv-minv
  cstep=1.D0/range !/255.D0

  write(*,"(a,1f16.6)")"minv  =",minv
  write(*,"(a,1f16.6)")"maxv  =",maxv
  write(*,"(a,1f16.6)")"range =",range
  write(*,"(a,1f16.6)")"cstep =",cstep


  open(2,file=ofile,status="replace")
  ! HEADER
  write(2,"(1a23)")"%!PS-Adobe-2.0 EPSF-2.0"
  write(2,"(1a14,4f16.6)")"%%BoundingBox:",pagellx,pagelly, pageurx,pageury

  ! FONT
  !------------------------------------------
  write(2,*)"/ReEncode {"
  write(2,*)"exch findfont"
  write(2,*)"dup length dict"
  write(2,*)"begin"
  write(2,*)"{"
  write(2,*)"1 index /FID eq"
  write(2,*)"{ pop pop }"
  write(2,*)"{ def } ifelse"
  write(2,*)"} forall"
  write(2,*)"/Encoding ISOLatin1Encoding def"
  write(2,*)"currentdict"
  write(2,*)"end"
  write(2,*)"definefont"
  write(2,*)"pop"
  write(2,*)"} bind def"
  write(2,*)"/Helvetica-Bold /HelveticaISO ReEncode"
  !------------------------------------------
  write(2,*)"/HelveticaISO findfont 14 scalefont setfont"


  ! BOXPLOT
  do y=1,ny


     do x=1,nx

        if(trim(colscheme)=="blue-yellow")then
           ! COLOR blue yellow
           r=(dat(x,y)-minv)*cstep
           !r=0.0
           g=1.D0
           b=1.0-r
           if(dat(x,y)==0.D0)then
              write(2,"(3f16.6,a)")1.D0, 1.D0, 1.D0," setrgbcolor"
           else
              write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
           endif
        endif

        if(trim(colscheme)=="green")then
           call hsbcolors(dat(x,y),r,g,b)
           write(2,"(3f16.6,a)")r, g, b," sethsbcolor"
        endif

        if(trim(colscheme)=="grey")then
           r = 1.d0 - (dat(x,y)/maxv) * 0.8
           g=r
           b=r
           write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
        endif

        write(2,"(a)")"newpath"
        write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen-ylen/2.D0," moveto"
        write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen-ylen/2.D0," lineto"
        write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen+ylen/2.D0," lineto"
        write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen+ylen/2.D0," lineto"
        write(2,"(a)")"closepath fill"

        write(2,"(a)")"newpath"
        write(2,"(1f16.6,a)")0.D0," setgray"
        write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen-ylen/2.D0," moveto"
        write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen-ylen/2.D0," lineto"
        write(2,"(2f16.6,a)")x*xlen+xlen/2.D0,y*ylen+ylen/2.D0," lineto"
        write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen+ylen/2.D0," lineto"
        write(2,"(a)")"closepath stroke"

        if(dat(x,y)==mv)then
           ! PLUS symbol
           !write(2,"(2f16.6,a)")x*xlen-xlen/4.D0,y*ylen," moveto"
           !write(2,"(2f16.6,a)")x*xlen+xlen/4.D0,y*ylen," lineto stroke"
           !write(2,"(2f16.6,a)")x*xlen,y*ylen-ylen/4.D0," moveto"
           !write(2,"(2f16.6,a)")x*xlen,y*ylen+ylen/4.D0," lineto stroke"
           ! CROSS
           write(2,"(1f16.6,a)")0.D0," setgray"
           !write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/3.d0,y*ylen-ylen/2+ylen/3.D0," moveto"
           !write(2,"(a)")"(x) show"
           write(2,"(2f16.6,a)")x*xlen-xlen/3.D0,y*ylen-ylen/3," moveto"
           write(2,"(2f16.6,a)")x*xlen+xlen/3.D0,y*ylen+ylen/3," lineto stroke"
           write(2,"(2f16.6,a)")x*xlen+xlen/3.D0,y*ylen-ylen/3.D0," moveto"
           write(2,"(2f16.6,a)")x*xlen-xlen/3.D0,y*ylen+ylen/3.D0," lineto stroke"

        endif

        if(shownumbers)then
           !if(dat(x,y)<0.7d0)then
           write(2,"(1f16.6,a)")0.D0," setgray"
           !else
           !   write(2,"(1f16.6,a)")1.D0," setgray"
           !endif
           if(dat(x,y)<1.d0)then
              write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/4.d0,y*ylen-ylen/2+ylen/3.D0," moveto"
              i=dat(x,y)*100.d0
              write(2,"(1a1,1i2.2,1a6)")"(",i,") show"
           endif
           !write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,y*ylen," moveto"
           !write(2,"(1a1,1f3.2,1a6)")"(",dat(x,y),") show"
        endif

        if(showsigsymbols.and.sig(x,y)>0)then
           write(2,*)"gsave"
           write(2,*)"/HelveticaISO findfont 20 scalefont setfont"
           write(2,"(1f16.6,a)")0.D0," setgray"
           select case (sig(x,y))
           case(1)
              write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/3.d0,y*ylen-ylen/2+ylen/8.D0," moveto"
              write(2,"(a)")"(*) show"
           case(2)
              write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/4.d0,y*ylen-ylen/2+ylen/8.D0," moveto"
              write(2,"(a)")"(**) show"
           case(3)
              !write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/8.d0, y*ylen-ylen/2+ylen/8.D0," moveto"
              !write(2,"(a)")"(***) show"

              write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/4.d0, y*ylen-ylen/2+ylen/20.D0," moveto"
              write(2,"(a)")"(**) show"
              write(2,"(2f16.6,a)")x*xlen-xlen/2.D0+xlen/2.8d0, y*ylen-ylen/2+ylen/4.D0," moveto"
              write(2,"(a)")"(*) show"


           end select
           write(2,*)"grestore"
        endif


     enddo
  enddo


  ! MOTHOD GROUP LINES
  do y=1,ny
     if( trim(ylabel(y))=="GWT".or.trim(ylabel(y))=="PXE".or.trim(ylabel(y))=="LND".or.trim(ylabel(y))=="HWD".or. &
          & trim(ylabel(y))=="KMN".or. trim(ylabel(y))=="MXG")then
        write(2,*)"gsave"
        write(2,"(3f16.6,a)")0.0, 0.0, 0.0," setrgbcolor"
        write(2,*)"3.0 setlinewidth"
        write(2,"(a)")"newpath"
        write(2,"(2f16.6,a)") 1*xlen-xlen/1.D0,y*ylen-ylen/2.D0," moveto"
        write(2,"(2f16.6,a)")nx*xlen+xlen/1.D0,y*ylen-ylen/2.D0," lineto"
        write(2,"(a)")"stroke"
        write(2,*)"grestore"
     endif
  enddo
  do x=1,nx
     if( trim(xlabel(x))=="GWT".or.trim(xlabel(x))=="PXE".or.trim(xlabel(x))=="LND".or.trim(xlabel(x))=="HWD".or. &
          & trim(xlabel(x))=="KMN".or. trim(xlabel(x))=="MXG")then
        write(2,*)"gsave"
        write(2,"(3f16.6,a)")0.0, 0.0, 0.0," setrgbcolor"
        write(2,*)"3.0 setlinewidth"
        !write(2,"(a)")"newpath"
        !write(2,"(2f16.6,a)") 1*xlen-xlen/1.D0,y*ylen-ylen/2.D0," moveto"
        !write(2,"(2f16.6,a)")nx*xlen+xlen/1.D0,y*ylen-ylen/2.D0," lineto"
        !write(2,"(a)")"stroke"
        write(2,"(a)")"newpath"
        write(2,"(2f16.6,a)")x*xlen-xlen/2.D0, 1*ylen-ylen/1.D0," moveto"
        write(2,"(2f16.6,a)")x*xlen-xlen/2.D0,ny*ylen+ylen/1.D0," lineto"
        write(2,"(a)")"stroke"
        write(2,*)"grestore"
     endif
  enddo


  ! METHOD LABELS
  write(2,"(1f16.6,a)")0.D0," setgray"
  ! Y-AXIS
  do y=1,ny,ytic
     write(2,"(2f16.6,a)")0*xlen-xlen, y*ylen-ylen/4.D0," moveto"
     !write(2,"(1a1,1f5.2,1a6)")"(",float(y),") show"
     !write(2,"(1a1,1i3,1a6)")"(",y,") show"
     write(2,"(1a1,a,1a6)")"(",trim(ylabel(y)),") show"
  enddo
  ! X-AXIS
  do x=1,nx,xtic
     !write(2,"(2f16.6,a)")x*xlen-xlen/2.D0, 1-ylen/2.D0," moveto"
     write(2,"(2f16.6,a)")x*xlen-xlen/5.D0, 1-ylen/10.D0," moveto"
     !write(2,"(1a1,1i3,1a6)")"(",x,") show"
     write(2,"(a)")"gsave -90 rotate"
     write(2,"(1a1,a,1a6)")"(",trim(xlabel(x)),") show"
     write(2,"(a)")"grestore"
  enddo

  ! LEGEND COLOR SCALE
  if(showlegend)then
     legheight=ny*ylen/2.D0
     legwidth=1*xlen
     legstep=legheight/(100.d0*maxv)
     !legstep=legheight/200
     !cstep=1.D0/200
     cstep=1.D0/(200.d0)
     !irange=aint(range)

     !xx=(nx+2)*xlen
     !do y=1,200
     !   !   ! COLORS
     !   !   r=y*cstep
     !   !   !r=0.0
     !   !   g=1.D0
     !   !   b=1.0-r
     !   !   write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
     !   call hsbcolors((y/100.d0)-1.d0,r,g,b)
     !   write(2,"(3f16.6,a)")r, g, b," sethsbcolor"
     !
     !   yy=legheight-legheight/2.D0 + y*legstep
     !   write(2,"(a)")"newpath"
     !   write(2,"(2f16.6,a)")xx-xlen/2.D0, yy," moveto"
     !   write(2,"(2f16.6,a)")xx+xlen/2.D0, yy," lineto"
     !   write(2,"(2f16.6,a)")xx+xlen/2.D0, yy+legstep," lineto"
     !   write(2,"(2f16.6,a)")xx-xlen/2.D0, yy+legstep," lineto"
     !   write(2,"(a)")"closepath fill"
     !enddo

     xx=(nx+2)*xlen
     iend=100*maxv+0.00000001
     do y=1,iend
        !   ! COLORS
        !   r=y*cstep
        !   !r=0.0
        !   g=1.D0
        !   b=1.0-r
        !   write(2,"(3f16.6,a)")r, g, b," setrgbcolor"

        ! call hsbcolors((y/100.d0)-1.d0,r,g,b)
        if(trim(colscheme)=="green")then
           call hsbcolors((y/100.d0),r,g,b)
           write(2,"(3f16.6,a)")r, g, b," sethsbcolor"
        endif

        if(trim(colscheme)=="grey")then
           r = 1.d0 - (y/100.d0) * 0.8
           g=r
           b=r
           write(2,"(3f16.6,a)")r, g, b," setrgbcolor"
        endif

        yy=legheight-legheight/2.D0 + y*legstep
        write(2,"(a)")"newpath"
        write(2,"(2f16.6,a)")xx-xlen/2.D0, yy," moveto"
        write(2,"(2f16.6,a)")xx+xlen/2.D0, yy," lineto"
        write(2,"(2f16.6,a)")xx+xlen/2.D0, yy+legstep," lineto"
        write(2,"(2f16.6,a)")xx-xlen/2.D0, yy+legstep," lineto"
        write(2,"(a)")"closepath fill"
     enddo


     ! LEGEND BOX
     !write(2,"(a)")"0.0 setgray"
     !write(2,"(a)")"newpath"
     !write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 1*legstep ," moveto"
     !write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 1*legstep," lineto"
     !write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 200*legstep+legstep," lineto"
     !write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 200*legstep+legstep," lineto"
     !write(2,"(a)")"closepath stroke"
     ! LEGEND BOX
     write(2,"(a)")"0.0 setgray"
     write(2,"(a)")"newpath"
     write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 1*legstep ," moveto"
     write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 1*legstep," lineto"
     write(2,"(2f16.6,a)")xx+xlen/2.D0, legheight-legheight/2.D0 + 100*maxv*legstep+legstep," lineto"
     write(2,"(2f16.6,a)")xx-xlen/2.D0, legheight-legheight/2.D0 + 100*maxv*legstep+legstep," lineto"
     write(2,"(a)")"closepath stroke"


     ! LEGEND LABELS
     !xx=(nx+2)*xlen+xlen/2.D0
     !do y=-100,100,20
     !   yy=legheight + y*legstep
     !
     !   write(2,"(2f16.6,a)")xx, yy," moveto"
     !
     !   write(2,"(1a1,1f6.2,1a6)")"(",y/100.D0,") show"
     !enddo
     ! LEGEND LABELS
     xx=(nx+2)*xlen+xlen/2.D0
     iend=100*maxv+0.00000001
     do y=0,iend,10

        ! LINES IN THE COLOR SCALE
        yy= legheight-legheight/2.D0 + (y+1)*legstep
        write(2,"(2f16.6,a)") (nx+1)*xlen+xlen/2.D0 , yy," moveto"
        write(2,"(2f16.6,a)") (nx+2)*xlen+xlen/2.D0 , yy," lineto stroke"


        !yy=legheight + y*legstep
        yy=legheight-legheight/2.D0 + y*legstep

        write(2,"(2f16.6,a)")xx, yy," moveto"

        write(2,"(1a1,1f6.2,1a6)")"(",y/100.D0,") show"
     enddo

  endif

  ! TITLE
  if(showtitle)then
     if( title /= "" )then
        write(2,"(a)")"newpath"
        write(2,"(2f16.6,a)")(nx+1)/2.D0*xlen,(ny+1)*ylen," moveto"
        write(2,"(a)")"("//trim(title)//") dup stringwidth pop 2 div neg 0 rmoveto show"
     endif
  endif


  write(2,"(a)")"showpage"
  close(2)


end subroutine pixmap


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  subroutine hsbcolors(val,h,s,b)
    implicit none
    real(kind=8) :: val,h,s,b,sval
    real(kind=8) :: minv,maxv,range
    real(kind=8) :: a,minsat,c

    minv=-1.D0
    maxv=+1.D0
    range=maxv-minv
    sval=(val-minv)/range

    a=0.75 ! Wert ab dem farbe dunkler wird
    minsat=0.5D0 ! Wert der minimalen saturation
    c=1.D0-a



    !h=0.33d3 ! red ! h=0 kennzeichnet Rot, h=1/6 Gelb, h=1/3 Gr\FCn, h=1/2 Cyan, h=2/3 Blau, h=5/6 Magenta (Purpur), und h=1 ist wieder Rot
    !h=0.7d0 ! blue
    h=0.33d0 ! green
    s=val
    b=1.d0

    return

!!!!!!!!!!!!!!! ANDI - SCALA

    if(sval<0.25)then
       h=0.D0
       s=1.D0
       b=1.D0
       sval=1.D0-sval
       b=((sval-0.75)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
    elseif(sval>a)then
       h=0.33D0
       s=1.D0
       !         a       b     c
       !b=((sval-0.75)*(0.5D0/0.25D0)+0.5)*-1.D0+1.5D0
       !b=((sval-a)*(minsat/c)+(1-minsat))*-1.D0+1.D0+minsat
       b=((sval-0.75)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
    else
       h=(sval-0.25)*(0.33333/0.5D0)
       s=(1.D0-h)-0.1
       b=1.D0
    endif

!!!!!!!!!!!!!!!!!!!!!!!!! Steffi-Skala

!!$  if(sval<0.5)then
!!$     h=0.D0
!!$     s=1.D0-sval
!!$     !b=1.D0
!!$     sval=1.D0-sval
!!$     b=((sval-0.5)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
!!$  elseif(sval>0.5)then
!!$     h=0.33D0
!!$     s=1.D0-h
!!$     b=((sval-0.5)*(0.5D0/0.25D0)+0.5)*(-1.D0)+1.5D0
!!$  else
!!$     !h=(sval-0.5)*(0.33333/0.5D0)
!!$     h=0.166
!!$     s=1.D0
!!$     b=1.D0
!!$  endif

!!!!!!!!!!!!!!!!!!!!!!!!!

!!$  if(sval>=0.5)then
!!$     h=(sval-0.5)*2
!!$  else
!!$     h=0.D0
!!$  endif
!!$
!!$  if(sval<=0.5)then
!!$     s=-1*(sval-0.5)*2
!!$  else
!!$     s=0.D0
!!$  endif
!!$
!!$  !g=1.D0-r
!!$
!!$  b=1.D0-abs((sval-0.5)*2)

  end subroutine hsbcolors
