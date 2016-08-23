      program DoubleBars
      implicit none
      integer NVAR,NMAX,KMAXX
      parameter(NVAR=8,NMAX=50,KMAXX=10000) !same values as in odeint
      integer kmax,kount,nok,nbad,i,j
      REAL ys(NVAR),xp(KMAXX),yp(NMAX,KMAXX),dxsav,x1,x2,ej,om,v0,a,q
     #,r(KMAXX),m1,m2,m3,m4
      external derivs,rkqs
      common /path/ kmax,kount,dxsav,xp,yp
      common /params/ om,v0,a,q
      x1=0.
      x2=6*4*ATAN(1.)

      m1 = 0.75
      m2 = 0.25
      m3 = 1.
      m4 = 1.

      ys(1) = -1*m2             ! m1,x
      ys(2) = 0                 ! m1,vx
      ys(3) = 0                 ! m1,y
      ys(4) = 1*m2              ! m1,vy

      ys(5) = 1*m1              ! m2,x
      ys(6) = 0                 ! m2,vx
      ys(7) = 0                 ! m2,y
      ys(8) = -1*m1             ! m2,vy
c
      call odeint(ys,nvar,x1,x2,1.e-7,1.e-8,0.0,nok,nbad,derivs,rkqs)
c
      do 10 i=1,kmax
         ej = 0.5*(yp(2,i)**2+yp(4,i)**2)-0.5*om*(yp(1,i)**2+yp(3,i)**2)
     #      + 0.5*v0**2*alog(a**2+yp(1,i)**2+(yp(3,i)/q)**2)
         r(i)=sqrt(yp(1,i)**2+yp(3,i)**2)
         write(9,33) xp(i), (yp(j,i),j=1,NVAR), ej,i,r(i)
         if(xp(i).eq.x2) goto 99
 10   continue
 99   continue
 33   format(9f10.4,f13.8,i5,f10.4)
      end
c
      subroutine derivs(x,y,dydx)
      implicit none
      integer NVAR
      PARAMETER(NVAR=8)         !same values as in main program
      real x,y(NVAR),dydx(NVAR),om,v0,a,q
      real dist1,dist2,dist3, dist4, dist5, dist6
      real r10,m1,m2
      COMMON /params/ om,v0,a,q
      m1=0.75
      m2=0.25
      dist1 = y(5)-y(1)         !x2-x1
      dist4 = y(7)-y(3)         !y2-y1
      r10=sqrt(dist1**2+dist4**2) 
      dydx(1) = y(2)  
      dydx(2) = (m2*dist1)/(r10**3)
      dydx(3) = y(4)
      dydx(4) = (m2*dist4)/(r10**3)
      dydx(5) = y(6)
      dydx(6) = (-m1*dist1)/(r10**3)
      dydx(7) = y(8)
      dydx(8) = (-m1*dist4)/(r10**3)
      return
      end
c
      SUBROUTINE odeint(ystart,nvar,x1,x2,eps,h1,hmin,nok,nbad,derivs,
     *rkqs)
      INTEGER nbad,nok,nvar,KMAXX,MAXSTP,NMAX
      REAL eps,h1,hmin,x1,x2,ystart(nvar),TINY
      EXTERNAL derivs,rkqs
      PARAMETER (MAXSTP=1000000,NMAX=50,KMAXX=10000,TINY=1.e-30)
      INTEGER i,kmax,kount,nstp
      REAL dxsav,h,hdid,hnext,x,xsav,dydx(NMAX),xp(KMAXX),y(NMAX),
     *yp(NMAX,KMAXX),yscal(NMAX)
      COMMON /path/ kmax,kount,dxsav,xp,yp
      kmax = KMAXX
      dxsav = (x2-x1)/float(kmax)
      x=x1
      h=sign(h1,x2-x1)
      nok=0
      nbad=0
      kount=0
      do 11 i=1,nvar
        y(i)=ystart(i)
11    continue
      if (kmax.gt.0) xsav=x-2.*dxsav
      do 16 nstp=1,MAXSTP
        call derivs(x,y,dydx)
        do 12 i=1,nvar
          yscal(i)=abs(y(i))+abs(h*dydx(i))+TINY
12      continue
        if(kmax.gt.0)then
          if(abs(x-xsav).gt.abs(dxsav)) then
            if(kount.lt.kmax-1)then
              kount=kount+1
              xp(kount)=x
              do 13 i=1,nvar
                yp(i,kount)=y(i)
13            continue
              xsav=x
            endif
          endif
        endif
        if((x+h-x2)*(x+h-x1).gt.0.) h=x2-x
        call rkqs(y,dydx,nvar,x,h,eps,yscal,hdid,hnext,derivs)
        if(hdid.eq.h)then
          nok=nok+1
        else
          nbad=nbad+1
        endif
        if((x-x2)*(x2-x1).ge.0.)then
          do 14 i=1,nvar
            ystart(i)=y(i)
14        continue
          if(kmax.ne.0)then
            kount=kount+1
            xp(kount)=x
            do 15 i=1,nvar
              yp(i,kount)=y(i)
15          continue
          endif
          return
        endif
        if(abs(hnext).lt.hmin) pause
     *'stepsize smaller than minimum in odeint'
        h=hnext
16    continue
      pause 'too many steps in odeint'
      return
      END
c
      SUBROUTINE rkqs(y,dydx,n,x,htry,eps,yscal,hdid,hnext,derivs)
      INTEGER n,NMAX
      REAL eps,hdid,hnext,htry,x,dydx(n),y(n),yscal(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
CU    USES derivs,rkck
      INTEGER i
      REAL errmax,h,htemp,xnew,yerr(NMAX),ytemp(NMAX),SAFETY,PGROW,
     *PSHRNK,ERRCON
      PARAMETER (SAFETY=0.9,PGROW=-.2,PSHRNK=-.25,ERRCON=1.89e-4)
      h=htry
1     call rkck(y,dydx,n,x,h,ytemp,yerr,derivs)
      errmax=0.
      do 11 i=1,n
        errmax=max(errmax,abs(yerr(i)/yscal(i)))
11    continue
      errmax=errmax/eps
      if(errmax.gt.1.)then
        htemp=SAFETY*h*(errmax**PSHRNK)
        h=sign(max(abs(htemp),0.1*abs(h)),h)
        xnew=x+h
        if(xnew.eq.x)pause 'stepsize underflow in rkqs'
        goto 1
      else
        if(errmax.gt.ERRCON)then
          hnext=SAFETY*h*(errmax**PGROW)
        else
          hnext=5.*h
        endif
        hdid=h
        x=x+h
        do 12 i=1,n
          y(i)=ytemp(i)
12      continue
        return
      endif
      END
c
      SUBROUTINE rkck(y,dydx,n,x,h,yout,yerr,derivs)
      INTEGER n,NMAX
      REAL h,x,dydx(n),y(n),yerr(n),yout(n)
      EXTERNAL derivs
      PARAMETER (NMAX=50)
CU    USES derivs
      INTEGER i
      REAL ak2(NMAX),ak3(NMAX),ak4(NMAX),ak5(NMAX),ak6(NMAX),
     *ytemp(NMAX),A2,A3,A4,A5,A6,B21,B31,B32,B41,B42,B43,B51,B52,B53,
     *B54,B61,B62,B63,B64,B65,C1,C3,C4,C6,DC1,DC3,DC4,DC5,DC6
      PARAMETER (A2=.2,A3=.3,A4=.6,A5=1.,A6=.875,B21=.2,B31=3./40.,
     *B32=9./40.,B41=.3,B42=-.9,B43=1.2,B51=-11./54.,B52=2.5,
     *B53=-70./27.,B54=35./27.,B61=1631./55296.,B62=175./512.,
     *B63=575./13824.,B64=44275./110592.,B65=253./4096.,C1=37./378.,
     *C3=250./621.,C4=125./594.,C6=512./1771.,DC1=C1-2825./27648.,
     *DC3=C3-18575./48384.,DC4=C4-13525./55296.,DC5=-277./14336.,
     *DC6=C6-.25)
      do 11 i=1,n
        ytemp(i)=y(i)+B21*h*dydx(i)
11    continue
      call derivs(x+A2*h,ytemp,ak2)
      do 12 i=1,n
        ytemp(i)=y(i)+h*(B31*dydx(i)+B32*ak2(i))
12    continue
      call derivs(x+A3*h,ytemp,ak3)
      do 13 i=1,n
        ytemp(i)=y(i)+h*(B41*dydx(i)+B42*ak2(i)+B43*ak3(i))
13    continue
      call derivs(x+A4*h,ytemp,ak4)
      do 14 i=1,n
        ytemp(i)=y(i)+h*(B51*dydx(i)+B52*ak2(i)+B53*ak3(i)+B54*ak4(i))
14    continue
      call derivs(x+A5*h,ytemp,ak5)
      do 15 i=1,n
        ytemp(i)=y(i)+h*(B61*dydx(i)+B62*ak2(i)+B63*ak3(i)+B64*ak4(i)+
     *B65*ak5(i))
15    continue
      call derivs(x+A6*h,ytemp,ak6)
      do 16 i=1,n
        yout(i)=y(i)+h*(C1*dydx(i)+C3*ak3(i)+C4*ak4(i)+C6*ak6(i))
16    continue
      do 17 i=1,n
        yerr(i)=h*(DC1*dydx(i)+DC3*ak3(i)+DC4*ak4(i)+DC5*ak5(i)+DC6*
     *ak6(i))
17    continue
      return
      END
