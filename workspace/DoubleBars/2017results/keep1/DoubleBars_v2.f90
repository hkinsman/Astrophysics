 program doublebars
   implicit none
   integer, parameter :: NVAR=16 ,NMAX=50,KMAXX=10000
   integer :: kmax=0,i,j, period1, period2, period3, period4
   real :: ys(NVAR),xp(KMAXX),yp(NMAX,KMAXX),m1,m2,m3,m4,m, start, end, step, energy(KMAXX), initialx1,initialx2
    real :: initialx3,initialx4
   real, parameter :: PI=4*ATAN(1.)

   call initialconditions
   call odeint(ys,start,end,step,1.e-8,0.0)

   do i=1,kmax
      call calcenergy
      write(9,33) xp(i), (yp(j,i),j=1,NVAR),i,energy(i)
      if(xp(i)==end) then
          exit
      end if
   end do
 33 format(17f10.4,i5,f10.4)

 contains

   subroutine initialconditions

     step = 1.e-7 !vary step size
     start=0.
     end=20 !vary end time

      m1=.5   !blue
      m2=.5  !red
      m3=0.09    !black
      m4=0.09   !yellow

     ys(1) = -.5     ! m1,x
     initialx1=ys(1)
     ys(2) = 0  ! m1,vx
     ys(3) = 0       ! m1,y
     ys(4) = -1		!m1,vy
     
     ys(5) = .5         ! m2,x
    initialx2=ys(5)
     ys(6) = 0          ! m2,vx
     ys(7) = 0         ! m2,y
     ys(8) = 1	!m2,vy

     ys(9) = -.106     ! m3,x
    initialx3=ys(9)
     ys(10) = 0       ! m3,vx
     ys(11) = 0        ! m3,y
     ys(12) = -.001		!m3, vy

     ys(13) = .106     ! m4,x
  !initialx4=ys(13)
     ys(14) = 0      ! m4,vx
     ys(15) = 0        ! m4,y
     ys(16) = .001		!m4, vy

   end subroutine initialconditions

   subroutine derivs(x,y,dydx)
     real, intent(in) :: x,y(NVAR)
     real, intent(out) :: dydx(NVAR)
     real :: dist1,dist2,dist3, dist4, dist5, dist6,dist7,dist8,dist9,dist10,dist11,dist12,r1,r2,r3,r4,r5,r6

    dist1 = y(5)-y(1)  !x2-x1
    dist2 = y(7)-y(3) !y2-y1
    dist3 = y(9)-y(1) !x3-x1
    dist4 = y(11)-y(3) !y3-y1
    dist5 = y(13)-y(1) !x4-x1
    dist6 = y(15)-y(3) !y4-y1
    dist7 = y(9)-y(5)    !x3-x2
    dist8 = y(11)-y(7)  !y3-y2
    dist9 = y(13)-y(5) !x4-x2
    dist10 =y(15)-y(7) !y4-y2
    dist11 =y(13)-y(9) !x4-x3
    dist12 =y(15)-y(11) !y4-y3

    r1=sqrt(dist1**2+dist2**2)  !r12
    r2=sqrt(dist3**2+dist4**2)  !r13
    r3=sqrt(dist5**2+dist6**2) !r14
    r4=sqrt(dist7**2+dist8**2)   !r23
    r5=sqrt(dist9**2+dist10**2)  !r24
    r6=sqrt(dist11**2+dist12**2) !r34

    dydx(1) = y(2) !dx1
    dydx(2) = ((m2*dist1)/(r1**3)) + ((m3*dist3)/(r2**3)) + ((m4*dist5)/(r3**3))! dvx1
    dydx(3) = y(4) !dy1
    dydx(4) = ((m2*dist2)/(r1**3)) + ((m3*dist4)/(r2**3)) + ((m4*dist6)/(r3**3)) ! dvy1
    dydx(5) = y(6) !dx2
    dydx(6) = ((-m1*dist1)/(r1**3)) + ((m3*dist7)/(r4**3)) + ((m4*dist9)/(r5**3)) !dvx2
    dydx(7) = y(8) !dy2
    dydx(8) = ((-m1*dist2)/(r1**3)) + ((m3*dist8)/(r4**3)) + ((m4*dist10)/(r5**3))   !dvy2
    dydx(9) = y(10) !dx3
    dydx(10) = ((-m1*dist3)/(r2**3)) + ((-m2*dist7)/(r4**3)) + ((m4*dist11)/(r6**3))   !dvx3
    dydx(11) = y(12) !dy3
    dydx(12) = ((-m1*dist4)/(r2**3)) + ((-m2*dist8)/(r4**3))  + ((m4*dist12)/(r6**3))   !dvy3
    dydx(13) = y(14) !dx4
    dydx(14) = ((-m1*dist5)/(r3**3)) + ((-m2*dist9)/(r5**3)) + ((-m3*dist11)/(r6**3))    !dvx4
    dydx(15) = y(16) !dy4
	dydx(16) = ((-m1*dist6)/(r3**3)) + ((-m2*dist10)/(r5**3)) + ((-m3*dist12)/(r6**3))        !dvx4

   end subroutine derivs

   subroutine calcenergy
   real :: vel1,vel2,vel3,vel4,distance,kinetic, potential

	vel1 = yp(2,i)**2+yp(4,i)**2
	vel2 = yp(6,i)**2+yp(8,i)**2
	vel3 = yp(10,i)**2+yp(12,i)**2
	vel4 = yp(14,i)**2+yp(16,i)**2
	distance  = sqrt(((yp(5,i)-yp(1,i))**2)+((yp(7,i)-yp(3,i)))**2)
	potential = (m2*m1)/distance
	distance  = sqrt(((yp(9,i)-yp(1,i))**2)+((yp(11,i)-yp(3,i)))**2)
	potential = potential+((m3*m1)/distance)
	distance  = sqrt(((yp(9,i)-yp(5,i))**2)+((yp(11,i)-yp(7,i)))**2)
	potential = potential+((m3*m2)/distance)
	distance  = sqrt(((yp(13,i)-yp(1,i))**2)+((yp(15,i)-yp(3,i)))**2)
	potential = potential+((m4*m1)/distance)
	distance  = sqrt(((yp(13,i)-yp(5,i))**2)+((yp(15,i)-yp(7,i)))**2)
	potential = potential+((m4*m2)/distance)
	distance  = sqrt(((yp(13,i)-yp(9,i))**2)+((yp(15,i)-yp(11,i)))**2)
	potential = potential+((m4*m3)/distance)

	kinetic= ((m1/2)*vel1)+((m2/2)*vel2)+((m3/2)*vel3)+((m4/2)*vel4) !-mp*ms/r
   	energy(i)=kinetic-potential
   end subroutine calcenergy

   subroutine odeint(ystart,x1,x2,eps,h1,hmin)
     real, intent(in) :: eps,h1,hmin,x1,x2
     real, intent(inout) :: ystart(nvar)
     integer :: kount,nok,nbad
     integer, parameter :: MAXSTP=1000000
     real, parameter :: TINY=1.e-30
     integer :: nstp
     real :: dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),y(NMAX),yscal(NMAX)

     kmax = KMAXX
     dxsav = (x2-x1)/float(kmax)
     x=x1
     h=sign(h1,x2-x1)
     nok=0
     nbad=0
     kount=0
     do i=1,nvar
        y(i)=ystart(i)
     end do
     if (kmax>0) then 
        xsav=x-2.*dxsav
     end if
     do nstp=1,MAXSTP
        call derivs(x,y,dydx)
        do i=1,nvar
           yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
        end do
        if(kmax>0)then
           if(abs(x-xsav)>abs(dxsav)) then
              if(kount<kmax-1)then
                 kount=kount+1
                 xp(kount)=x
                 do i=1,nvar
                    yp(i,kount)=y(i)
                 end do
                 xsav=x
              end if
           end if
        end if
        if((x+h-x2)*(x+h-x1)>0.) then
           h=x2-x
        end if
        call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext)
        if(hdid==h) then
           nok=nok+1
        else
           nbad=nbad+1
        end if
        if((x-x2)*(x2-x1)>=0.) then
           do i=1,nvar
              ystart(i)=y(i)
           end do
           if(kmax/=0) then
              kount=kount+1
              xp(kount)=x
              do i=1,nvar
                 yp(i,kount)=y(i)
              end do
           end if
           return
        end if
        if(abs(hnext)<hmin) then
           print *,'stepsize smaller than minimum in odeint'
           read(*,*)
           stop
        end if
        h=hnext
     end do
     print *, 'too many steps in odeint'
     read(*,*)
     stop
   end subroutine odeint

   subroutine rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext)
     real, intent(in) :: dydx(n),eps, htry
     real, intent(inout) :: y(n),x, hnext, hdid
     integer, intent(in) :: n
     real :: yscal(n)
     integer :: i
     real :: errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX)
     real, parameter :: SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89e-4
     h=htry
1    call rkck(y,dydx,n,x,h,ytemp,yerr)
     errmax=0.
     do i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
     end do
     errmax=errmax/eps
     if(errmax>1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1*abs(h)),h)
        xnew=x+h
        if(xnew==x) then  
           print *, 'stepsize underflow in rkqs'
           read(*,*)
           stop
        end if
        goto 1
     else
        if(errmax>ERRCON) then
           hnext=SAFETY*h*(errmax**PGROW)
        else
           hnext=5.*h
        endif
        hdid=h
        x=x+h
        do i=1,n
           y(i)=ytemp(i)
        end do
        return
     endif
   end subroutine rkqs

   subroutine rkck(y,dydx,n,x,h,yout,yerr)
     real,intent(out) :: yerr(n),yout(n)
     real, intent(in) :: h,x,dydx(n),y(n)
     integer,intent(in) :: n
     integer :: i
     real :: ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX), ytemp(NMAX)

     real, parameter :: A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40., &
          B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5, &
          B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512., &
          B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378., &
          C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648., &
          DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336., &
          DC6=C6-.25
     do i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
     end do
     call derivs(x+A2*h,ytemp,ak2)
     do i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
     end do
     call derivs(x+A3*h,ytemp,ak3)
     do i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
     end do
     call derivs(x+A4*h,ytemp,ak4)
     do i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
     end do
     call derivs(x+A5*h,ytemp,ak5)
     do i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+B65*ak5(i))
     end do
     call derivs(x+A6*h,ytemp,ak6)
     do i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
     end do
     do i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*ak6(i))
     end do
   end subroutine rkck
end program doublebars
