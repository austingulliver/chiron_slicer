      if(.not.hardcopy)then
          write(6,1100)
 1100     format(1x,'OPTIONS: <X>S% HBP M=measure anew, 2=Return to 2-D plot,E=End ',$)
          READ(5,102)IRES
          ires_save=ires
      end if
      ires=ires_save
      if(ires.eq.'P'.or.ires.eq.'p')go to 87
  102 FORMAT(A1)
      IF(IRES.EQ.'<')GO TO 71
      IF(IRES.EQ.'>')GO TO 70
      IF(IRES.EQ.'X'.or.ires.eq.'x')GO TO 72
      IF(IRES.EQ.'S'.or.ires.eq.'s')GO TO 73
      IF(IRES.EQ.'T'.or.ires.eq.'t')GO TO 66
      IF(IRES.EQ.'%')GO TO 74
      IF(IRES.EQ.'H'.and.hardcopy.eq..false..or.ires.eq.'h'.and.hardcopy.eq..false.)then
          call makehard('/CPS')
          write(6,*)'Calling makehard'
          write(6,*)device_type,length_device
          ihardcopy=2
          go to 86
      end if
      IF(IRES.EQ.'B'.OR.IRES.EQ.'C'.OR.IRES.EQ.'N')CALL PGPAGE
      if(ires.eq.'b'.or.ires.eq.'c'.or.ires.eq.'n')call pgpage
      IF(IRES.EQ.'B'.OR.IRES.EQ.'C'.OR.IRES.EQ.'N')WRITE(3,776)
      if(ires.eq.'b'.or.ires.eq.'c'.or.ires.eq.'n')write(3,776)
      IF(IRES.EQ.'B'.or.ires.eq.'b')GO TO 52
      IF(IRES.EQ.'M'.or.ires.eq.'m')GO TO 52
      if(ires.eq.'2')return
      if(ires.eq.'E'.or.ires.eq.'e')then
          iend_now=1
      end if
      IF(IRES.NE.'E'.AND.ires.NE.'e')GO TO 64
      GO TO 31
   70 lo=lo-NPTS/IFRAC
      lh=lh-NPTS/IFRAC
      GO TO 86
   71 lo=lo+NPTS/IFRAC
      lh=lh+NPTS/IFRAC
      GO TO 86
   72 lo=lo-NPTS/IFRAC
      lh=lh+NPTS/IFRAC
      GO TO 86
   73 lo=lo+NPTS/IFRAC
      lh=lh-NPTS/IFRAC


      GO TO 86
 !  74 TYPE *,'ENTER % INCREASE FOR PLOT (INTEGER)' ! THis was uncommented at the beginning 
      READ(5,*,ERR=74)IFRAC
      GO TO 86
c P command enters this section




   87 continue
c     XM_temp=DLAM-FLOAT(LOS)  
      xm_temp=dlam
C     Round off to nearest integer
      jm=dlam + 0.5
D	write(50,*)
D	write(50,*)' Profile generation, (xxl,yyl), centred on jm'
D	write(50,*)' k=',k,' dlam=',dlam,' yback=',yback,' ycentre=',
D	1	ycentre
D	write(50,*)' lo=',lo,' lh=',lh,' xm=',xm_temp,' jm=',jm
C     Generate (xxl,yyl) centred on JM = DLAM
      do  i=1,nPTS
          xxl(i)=float(i+lo-1-jm)
          yyl(i)=y(i+LO-1)
C         write(3,*)i,xxl(i),yyl(i)
D         write(50,*)' i=',i,' xxl=',xxl(i),' yyl=',yyl(i)
      end do
C     Round off to the nearest integer
      jm=xm_temp + 0.5
D	write(50,*)
D	write(50,*)' Shift profile to centre on xm in (xxdum,y)',
D	1	' then (xxl,yyl)'
D	write(50,*)' xm=',xm_temp,' jm=',jm '
C     Shift profile to centre on XM by interpolating into array Y
      do i=1,npts
          x=xm_temp-float(jm)+xxl(i)
          call intep(x,y(i),xxl,yyl,npts,ier)
          xxdum(i)=float(-jm+i)
C         write(3,*)i,jm,xxdum(i),y(i),xxl(i)
D	  write(50,*)' i=',i,' x=',x,' xxdum=',xxdum(i),' xxl=',xxl(i)
D	  write(50,*)' y=',y(i)
      end do
C     Re-normalize and store shifted profile in XXL and YYL
      ymax=0.
      ymin=1000000.
      do i=1,npts
          if(y(i).gt.ymax)then
              ymax=y(i)
          else if(y(i).lt.ymin)then
              ymin=y(i)
          end if
      end do
      do i=1,npts
         yyl(i)=(y(i)-ymin)/(ymax-ymin)
      end do
      ymax=1.
      ymin=0.
      fwhm=fw(xxdum,yyl,npts,ymin,ymax)
      do i=1,npts
          xxl(i)=xxdum(i)*2./fwhm
      end do
      xl = xxl(1)
C     write(3,*)fwhm
      xm_temp=jm
      XH=xxl(npts)
      xoff=0.02*(xh-xl)
D	write(50,*)
D	write(50,*)' Plot x/fwhm axis'
D	write(50,*)' fwhm=',fwhm,' for yback=',yback,' ycentre=',ycentre
D	write(50,*)' xl=',xl,' xm=',xm_temp,' xh=',xh
      call pgpage
      CALL pgvport(xleft,xright,ybottom,ytop)
      call pgmtxt('L',4.0,0.5,0.5,'Intensity in Y')
      call pgmtxt('B',2.5,0.5,0.5,'x/hwhm')
      call pgiden
      yoff=0.02*(ymax-ymin)
      xoff=2.
      call pgwindow(xl-xoff,xh+xoff,ymin-yoff,ymax+yoff,0,0)
      call pgbox('BNSTC',0.0,0,'BNSTCV',0.0,0)
      call pgmtxt('T',0.8,0.7,0.0,com)
      call pgslw(2)
      call pgmtxt('T',2.4,0.8,0.0,'in FORMPRO')
      call pgmtxt('T',0.8,0.0,0.0,file_name_com)
      call pgslw(1)
c     call pgmtxt('T',0.8,0.0,0.0,fileheader)
      DO I=1,npts
          x_val=xxl(i)
          y_val=yyl(i)
          call pgpt(1,x_val,y_val,0)
      end do 






  400 format('x/hwhm  low ',f8.3)
  409 format(1x,a20)
      write(com,400)xl
      read(com,410)zlo
  410 format(a20)
      write(com,401)xh
      read(com,410)zhi
C      write(3,409)zhi
  401 format('x/hwhm  high',f8.3)
  402 format('FWHM data pts ',f5.2)
  403 format('Cont ht   ',f10.3)
      write(com,403)yback
      read(com,410)bak
  404 format('Line max   ',f9.3)
      write(com,404)ycentre
      read(com,410)dep
      write(com,402)fwhm
      read(com,410)fwh
      call pgmtxt('T',-5.,0.70,0.0,zlo)
      call pgmtxt('T',-6.,0.70,0.0,zhi)
      call pgmtxt('T',-7.,0.70,0.0,bak)
      call pgmtxt('T',-8.,0.70,0.0,dep)
      call pgmtxt('T',-9.,0.7,0.0,fwh)
      dont_fold_profile=.true.
      if(hardcopy)then
          call pgbegin(0,DeVICE_TYPE(1:LENGTH_DEVICE),1,1)
          hardcopy=.false.
      end if
  85  if(.not.hardcopy)then
          write(6,1144)
 1144     format(1x,'H=hard copy, A=store; 2=Return to 2-D plot,E=To MAIN ',$)
          read(5,22)ians
          ians_save=ians
      end if
      ians=ians_save
      if(ians.eq.'F'.or.ians.eq.'f')then
          fold_profile=.true.
          go to 88
      else if(ians.eq.'E'.or.ians.eq.'e')then
          iend_now=1
          return
      else if(ians.eq.'2')then
          return
      else if(ians.eq.'H'.or.ians.eq.'h')then
          call makehard('/CPS')
          write(6,*)'Calling makehard'
          write(6,*)device_type,length_device
          ihardcopy=3
          go to 86
c         go to 17
      else if(ians.NE.'A'.AND.ians.NE.'a')then
          go to 85
      end if
      yback=0.
      YCENTRE=1.
      write(6,1145)
 1145 format(1x,'Enter file name (use psf.std as default) ',$)
      read(5,23)std_line_file
      open(33,file=std_line_file,status='NEW',disp='KEEP')
      dx=.025
      ddx=.1
      dsig=.025
      ixl=xl
      ixh=xh
      write(33,7100)file_name_com
 7100 format(1x,'FILENAME: ',a)
      write(33,250)yback,ycentre
D	write(50,*)' yback=',yback,' ycentre=',ycentre
      write(33,250)dx,dsig
  250 format(1x,2f7.3)
      xm_temp=0.
  251 format(1x,f8.3,2F8.3,f8.3)
C     Convert XXL to 'nice' values
      ixl=(10.*xxl(1))
      ixh=(10.*xxl(npts))
      XL=float(IXL)*.1
      XH=float(IXH)*.1
      write(33,251)xm_temp,Xl,Xh,ddx
D	write(50,*)
D	write(50,*)' Output file '
D	write(50,*)' xm=',xm_temp,' xl=',xl,' xh=',xh,' ddx=',ddx
      j=0
      iout = 0
c
C     Interpolate YYL at intervals of 0.1 of HWHM
c
  430 j=j+1
      x=xl+float(j-1)*0.1
      if(x.gt.xh)go to 431
      call intep(x,val,xxl,yyl,npts,ier)
      xxout(j) = x
      yyout(j) = val
      iout = iout + 1
C      write(33,255)x,val
  255 format(1x,f7.3,f8.4)
      go to 430
c
C Write to output profile file
c
  431 do i = 1,iout
         write(33,255)xxout(i),yyout(i)
D	 write(50,*)' xout=',xxout(i),' yout=',yyout(i)
      end do
      call pgslw(2)
      call pgmtxt('T',-3.,0.7,0.,std_line_file)
      call pgslw(1)
      call stline
      close(33,disp='keep')
 1152 write(6,1153)                   
 1153 format(1x,'What next? H=hard copy, S=To redo psf, 2=return to 2-D plot,E=To MAIN ',$)
      read(5,22)ians                                     
