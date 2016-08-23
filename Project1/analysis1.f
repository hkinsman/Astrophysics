      program LKBiasAnalyse

      implicit real*8 (a-h,o-z)

      open(unit=2, file='LK.dat')
      open(unit=3, file='LKRDist.dat',status='REPLACE')
      open(unit=15, file='LKTrueDist.dat',status='REPLACE')
      do 10 i=0, 12
         read(2,*)
 10      continue
         
      rmean=0
      count=0
      total=0
      temp=0
      stdev=0
      count1=0
      count2=0
      count3=0
      count4=0
c      count5=0
c      count6=0
      total1=0
      total2=0
      total3=0
      total4=0
c      total5=0
c      total6=0
      rmean1=0
      rmean2=0
      rmean3=0
      rmean4=0
c      rmean5=0
c      rmean6=0
      stdev1=0
      stdev2=0
      stdev3=0
      stdev4=0
c      stdev5=0
c      stdev6=0
         
      do 20 i=0, 400001
         read(2,*,IOSTAT=io) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
      if(io<0) then
         exit
      elseif(rmag.lt.99) then
         if(rdist.lt.4000) then
            count=count+1
            total=total+rmag
            write(3,*) tpara, tdist, appmag, obpara, paraerr,
     +           rdist, tmag, rmag, errphoto
         
            if(tdist.lt.4000) then
               write(15,*) tpara, tdist, appmag, obpara, paraerr,
     +              rdist, tmag, rmag, errphoto  
            endif
        endif
      endif
 20   continue   
      close(2)
      close(3)
      close(15)
      
      open(unit=4, file='LKRDist.dat')
      open(unit=5, file='Bin1000.dat',status='REPLACE')
      open(unit=7, file='Bin2000.dat',status='REPLACE')
      open(unit=8, file='Bin3000.dat',status='REPLACE')
      open(unit=9, file='Bin4000.dat',status='REPLACE')
c      open(unit=16, file='Bin5000.dat',status='REPLACE')  
c      open(unit=17, file='Bin6000.dat',status='REPLACE')    
     
      rmean=total/count
      do 30 i=0, count-1
         read(4,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         temp=temp+(rmag-rmean)**2
         if(rdist.le.1000) then
            count1=count1+1
            total1=total1+rmag
            write(5,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         elseif(rdist.le.2000) then
           total2=total2+rmag
            count2=count2+1
            write(7,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         elseif(rdist.le.3000) then
           total3=total3+rmag
            count3=count3+1
            write(8,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         elseif(rdist.le.4000) then
           total4=total4+rmag
            count4=count4+1
            write(9,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
c         elseif(rdist.le.5000) then
c           total5=total5+rmag
c            count5=count5+1
c            write(16,*) tpara, tdist, appmag, obpara, paraerr, 
c     +rdist, tmag, rmag, errphoto
c         elseif(rdist.le.6000) then
c           total6=total6+rmag
c            count6=count6+1
c            write(17,*) tpara, tdist, appmag, obpara, paraerr, 
c     +rdist, tmag, rmag, errphoto
         end if
 30   continue

      close(4)
      close(5)
      close(7)
      close(8)
      close(9)
c      close(16)
c      close(17)
      
      stdev=sqrt(temp/count)
      rmean1 = total1/count1
      rmean2 = total2/count2
      rmean3 = total3/count3
      rmean4 = total4/count4
c      rmean5 = total5/count5
c      rmean6 = total6/count6

      open(unit=10, file='Bin1000.dat')
      open(unit=11, file='Bin2000.dat')
      open(unit=12, file='Bin3000.dat')
      open(unit=13, file='Bin4000.dat') 
c      open(unit=18, file='Bin5000.dat') 
c      open(unit=19, file='Bin6000.dat') 
      
      temp=0
      do 40 i=0, count1-1
         read(10,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         temp=temp+(rmag-rmean1)**2
 40   continue
      close(10)
      stdev1=sqrt(temp/count1)

      temp=0
      do 50 i=0, count2-1
         read(11,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         temp=temp+(rmag-rmean2)**2
 50   continue
      close(11)
      stdev2=sqrt(temp/count2)

      temp=0
      do 60 i=0, count3-1
         read(12,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         temp=temp+(rmag-rmean3)**2
 60   continue
      close(12)
      stdev3=sqrt(temp/count3)

      temp=0
      do 70 i=0, count4-1
         read(13,*) tpara, tdist, appmag, obpara, paraerr, 
     +rdist, tmag, rmag, errphoto
         temp=temp+(rmag-rmean4)**2
 70   continue
      close(13)
      stdev4=sqrt(temp/count4)
     
c      temp=0
c      do 80 i=0, count5-1
c         read(18,*) tpara, tdist, appmag, obpara, paraerr, 
c     +rdist, tmag, rmag, errphoto
c         temp=temp+(rmag-rmean5)**2
c 80   continue
c      close(18)
c      stdev5=sqrt(temp/count5)

c      temp=0
c      do 90 i=0, count6-1
c         read(19,*) tpara, tdist, appmag, obpara, paraerr, 
c     +rdist, tmag, rmag, errphoto
c         temp=temp+(rmag-rmean6)**2
c 90   continue
c      close(19)
c      stdev6=sqrt(temp/count6)


      open(unit=14, file='Results.dat',status='REPLACE')
      write(14,*) 'Count=' , count
      write(14,*) 'Total=', total
      write(14,*) 'Mean=', rmean
      write(14,*) 'Standard Deviation=', stdev

      write(14,*) '0-1000 Mean=', rmean1
      write(14,*) '0-1000 Std Dev=', stdev1
      write(14,*) '1001-2000 Mean=', rmean2
      write(14,*) '1001-2000 Std Dev=', stdev2
      write(14,*) '2001-3000 Mean=', rmean3
      write(14,*) '2001-3000 Std Dev=', stdev3
      write(14,*) '3001-4000 Mean=', rmean4
      write(14,*) '3001-4000 Std Dev=', stdev4
c      write(14,*) '4001-5000 Mean=', rmean5
c      write(14,*) '4001-5000 Std Dev=', stdev5
c      write(14,*) '5001-6000 Mean=', rmean6
c      write(14,*) '5001-6000 Std Dev=', stdev6
      close(14)
      write(*,*) 'Finished!'
      stop

      end 
