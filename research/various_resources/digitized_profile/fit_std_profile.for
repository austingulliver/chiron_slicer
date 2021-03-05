c
c  The problem with a FT technique is that zero weighted points cannot be included
c  To allow the FT to iterate, zero weighted points can be replaced with:
c        1)  an updated avg value (this is the preferred method for the scattered light but 
c            sometimes produces spurious minima in the stellar spectrum fit at the location of CRs), 
c        2)  a FT derived yfit value (this slows the convergence of the CR removal)
c        3)  a linearly interpolated value derived from the nearest nonzero weighted points;
c            better results are achieved by starting the search for a nonzero point no less than 3
c            pixels away.
c
      npt=irow_high-irow_low+1
      do i=irow_low,irow_high
          if(icycle.ne.0.and.wt(i).eq.0)then
              if(scan_dark)then
                  y_temp(i-irow_low+1)=avg
c For psf fitting to star column there is no need to replace y values for wt 0 points and it will cause excessive spike rejection 
c if on second or subsequent passes (icycle>1) then replace y_temp with yfit for wt 0 to remove points from consideration for max
c and fwhm etc
              else
c                 y_temp(i-irow_low+1)=y(i)
                  y_temp(i-irow_low+1)=yfit(i)
              end if
              wt_temp(i-irow_low+1)=wt(i)
              iresurrect_temp(i-irow_low+1)=iresurrect(i)
          else
              y_temp(i-irow_low+1)=y(i)
              wt_temp(i-irow_low+1)=wt(i)
              iresurrect_temp(i-irow_low+1)=iresurrect(i)
          end if
      end do
c  PSF file should already be open
      if(fit_std_profile)then
          go to 1896
      else
 1897     continue
d         write(6,*)'Opening point spread function file ',std_line_file
          open(33,file=std_line_file,status='old',err=1899)
          go to 1898
 1899     write(6,*)'Error opening file ',std_line_file,' in PLOT_EDIT_COLUMN_AUTO'
          write(6,*)'Enter name of point spread function file'
          read(5,*)std_line_file
          go to 1897
 1898     call stline
      end if
 1896 fit_std_profile=.true.
      if(i_col.eq.i_col_test)write(6,*)'Fitting a standard profile or psf to the spectrum cross-section '
      ymax=fmax2(y_temp,imax2,npt)
      ymin=fmin2(y_temp,imin2,npt)
c     write(6,*)'ymin,ymax',ymin,ymax
c     imax2=imax2+irow_low-1
c     imin2=imin2+irow_low-1
d     write(6,*)'imin2,imax2',imin2,imax2
      half_intensity=(ymax-ymin)*0.5+ymin
      write(6,*)'ymin,ymax,half intensity',ymin,ymax,half_intensity
c Do not include 0 wt points
      do j=1,npt
          if(wt(j).ne.0.)then
              if(y_temp(j).gt.half_intensity)then
                  iy_lo_fwhm=j-1
d                 write(6,*)'Lo y index for FWHM ',iy_lo_fwhm
                  js=j
                  go to 3120
              end if
          end if
      end do
c3120 do j=js,npt
 3120 do j=npt,js,-1
          if(wt(j).ne.0.)then
              if(y_temp(j).gt.half_intensity)then
                  iy_hi_fwhm=j
                  write(6,*)'Hi y index for FWHM ',iy_hi_fwhm
                  go to 3121
              end if
          end if
      end do
 3121 iy_fwhm=(iy_hi_fwhm-iy_lo_fwhm)
c$$$ calculate location of max as midpoint between two half intensity points - this allows for double peak in spectrum psf
      imax2=(iy_hi_fwhm+iy_lo_fwhm)/2
      write(6,*)'Full width half maximum is ',iy_fwhm,' centered at point ',imax2+irow_low-1
      write(6,*)'from',iy_lo_fwhm+irow_low-1,' to',iy_hi_fwhm+irow_low-1,' with min',ymin,' and max',ymax
d     write(6,*)'y_temp from iy_lo_fwhm-2 to iy_hi_fwhm+2'
d     write(6,*)(y_temp(j),j=iy_lo_fwhm-2,iy_hi_fwhm+2)
d     write(6,*)'Complete column'
d     write(6,*)(y_temp(j),j=1,npt)
      bval(4)=(y_temp(npt)-y_temp(1))/(npt-1)
      par_values(4)=bval(4)
      write(6,*)'profile slope par_values(4)= ',par_values(4)
      bval(5)=0.5*(y_temp(1)+y_temp(npt))
      par_values(5)=bval(5)
      par_values(1)=ymax-ymin
      write(6,*)'profile height and background level = ', par_values(1),par_values(5)
      mode=1
      par_values(2)=float(imax2)
      par_values(3)=float(iy_hi_fwhm-iy_lo_fwhm)*0.5
      write(6,*)'max loc and hwhm par_values(2),par_values(3)= ', par_values(2),par_values(3)
      y_mean=bval(2)
      npar=5
      ktot=0
      ipp(4)=0
      ipp(5)=0
      ii=0
      do i=1,npar
          if(i.eq.4)then
              ii=ii+1
              indice(ii)=i
              par_values(ii)=(y_temp(irow_low+1)-y_temp(irow_high-2))/float(npt-4)
              bval(4)=par_values(ii)
          else if(i.eq.5)then
              ii=ii+1
              indice(ii)=i
              par_values(ii)=0.5*(y_temp(irow_low+1)+y_temp(irow_high-2))
              bval(5)=par_values(ii)
          else if(i.eq.1)then
              ii=ii+1
              indice(ii)=i
              bval(i)=par_values(ii)
          else if(i.gt.1.and.i.le.3)then
              ii=ii+1
              indice(ii)=i
              bval(i)=par_values(ii)
          end if
          jindex(i)=0
          kindex(i)=0
          deltaa(i)=0.
      end do
      ntermm=ii
      nterm=ntermm
d     write(3,*)'Indices ',(indice(i),i=1,ntermm)
d     write(6,*)'Indices ',(indice(i),i=1,ntermm)
d     write(3,*)'Par_values ',(par_values(i),i=1,ntermm)
d     write(3,*)'Bval       ',(bval(indice(i)),i=1,ntermm)
d     write(3,*)'Param ',(bval(i),i=1,npar)
      write(6,*)'Par_values ',(par_values(i),i=1,ntermm)
      write(6,*)'Bval       ',(bval(indice(i)),i=1,ntermm)
      write(6,*)'Param ',(bval(i),i=1,npar)
      ipp(1)=4
      ipp(2)=4
      ipp(3)=4
      ipp(4)=0
      ipp(5)=0
      ipp(6)=1
      ipp(7)=1
      ipp(8)=1
      flambda=0.001
      write(6,*)ntermm,' input parameters to CURFIT'
d     write(3,*)ntermm,' input parameters to CURFIT'
      do i=1,ntermm
          write(6,*)par_values(i),indice(i),ipp(i)
d         write(3,*)par_values(i),indice(i),ipp(i)
      end do
c     write(6,*)'Before curfit for',npt,'points in column',i_col
c     write(6,902)
  902 format(1x,' x      y_temp   wt_temp ires_temp')
c     do i=1,npt
c         x_ccd=i+irow_low-1
c         write(6,903)x_ccd,y_temp(i),wt_temp(i),iresurrect_temp(i)
  903     format(1x,f4.0,2x,f8.3,6x,f2.0,5x,i2)
c     end do
c     do ii=1,npt
c         x_ccd=ii+irow_low-1
c         write(6,*)x_ccd,y_temp(ii),wt_temp(ii),iresurrect(ii)
c     end do
      write(6,*)'Fitting psf to profile'
      fit_type='Psf       '
      call curfit(x,y_temp,wt_temp,npt,ntermm,mode,par_values,deltaa,sigma_values,flambda,yfit_temp,chisqr)
      y_mean=par_values(2)
      y_error=sigma_values(2)
d     write(3,*)'i_col,ymean,yerror ',i_col,y_mean,y_error
d     write(6,*)'i_col,ymean,yerror ',i_col,y_mean,y_error
c
c     call fit_small(x,y_temp,wt,yfit,rms_temp,npt,i)
c     call smfft(x,y_temp,yfit,npt,frac,rms_temp)
c     write(6,*)(y_temp(jl),jl=1,npt)
c     write(6,*)(yfit(jl),jl=1,npt)
c
c             This option needs to be better explored, that is fitting over a larger
c             range and then taking a subset of the fit.  The technique requires:
c                  Generation of a wide spatial file
c                  Setting of wider y-limits than normal
c                  Selecting regular limits for extracting the spectrum
c             Complication:  The lamp normalizing limits the effectiveness -
c                            Not solved this one yet.
c                            Experimenting to see how far y_limits can be stretched before
c                            get divide by zero in lamp normalization.
c 
c
c Temporarily set rms_temp to chisqr for omc_ratio calculation
      rms_temp=chisqr
      write(6,*)'After fit in column',i_col,npt,' values'
      if(i_col.eq.i_col_test)write(6,*)'chisqr from curfit is',chisqr
      write(6,905)i_col
  905 format(1x,' x       y_temp     yfit_temp      omc     omc rat   wt_temp ires_temp results in column',i4)
      do i=1,npt
          x_ccd=x(i)+irow_low-1
          omc=y_temp(i)-yfit_temp(i)
          omc_ratio=omc/(times_sigma_spline*rms_temp)
          write(6,901)x_ccd,y_temp(i),yfit_temp(i),omc,omc_ratio,wt_temp(i),iresurrect_temp(i)
      end do
c     do ii=irow_low,irow_high
c         x_ccd=ii
c         write(6,*)x_ccd,y_temp(ii-irow_low+1),yfit_temp(ii-irow_low+1),wt_temp(ii-irow_low+1),iresurrect(ii-irow_low+1)
c     end do
c shift temp resultant arrays back to irow_low to irow_high
      do ii=1,npt
c maintain zero wt values of y
c         y(ii+irow_low-1)=y_temp(ii)
          yfit(ii+irow_low-1)=yfit_temp(ii)
          wt(ii+irow_low-1)=wt_temp(ii)
          iresurrect(ii+irow_low-1)=iresurrect_temp(ii)
      end do
c
c  The problem with a FT technique is that zero weighted points cannot be included
c  To allow the FT to iterate, zero weighted points can be replaced with:
c        1)  an updated avg value (this is the preferred method for the scattered light but 
c            sometimes produces spurious minima in the stellar spectrum fit at the location of CRs), 
c        2)  a FT derived yfit value (this slows the convergence of the CR removal)
c        3)  a linearly interpolated value derived from the nearest nonzero weighted points;
c            better results are achieved by starting the search for a nonzero point no less than 3
c            pixels away.
c
      npt=irow_high-irow_low+1
      do i=irow_low,irow_high
          if(icycle.ne.0.and.wt(i).eq.0)then
              if(scan_dark)then
                  y_temp(i-irow_low+1)=avg
c For psf fitting to star column there is no need to replace y values for wt 0 points and it will cause excessive spike rejection 
c if on second or subsequent passes (icycle>1) then replace y_temp with yfit for wt 0 to remove points from consideration for max
c and fwhm etc
              else
c                 y_temp(i-irow_low+1)=y(i)
                  y_temp(i-irow_low+1)=yfit(i)
              end if
              wt_temp(i-irow_low+1)=wt(i)
              iresurrect_temp(i-irow_low+1)=iresurrect(i)
          else
              y_temp(i-irow_low+1)=y(i)
              wt_temp(i-irow_low+1)=wt(i)
              iresurrect_temp(i-irow_low+1)=iresurrect(i)
          end if
      end do
c  PSF file should already be open
      if(fit_std_profile)then
          go to 1896
      else
 1897     continue
d         write(6,*)'Opening point spread function file ',std_line_file
          open(33,file=std_line_file,status='old',err=1899)
          go to 1898
 1899     write(6,*)'Error opening file ',std_line_file,' in PLOT_EDIT_COLUMN_AUTO'
          write(6,*)'Enter name of point spread function file'
          read(5,*)std_line_file
          go to 1897
 1898     call stline
      end if
 1896 fit_std_profile=.true.
      if(i_col.eq.i_col_test)write(6,*)'Fitting a standard profile or psf to the spectrum cross-section '
      ymax=fmax2(y_temp,imax2,npt)
      ymin=fmin2(y_temp,imin2,npt)
c     write(6,*)'ymin,ymax',ymin,ymax
c     imax2=imax2+irow_low-1
c     imin2=imin2+irow_low-1
d     write(6,*)'imin2,imax2',imin2,imax2
      half_intensity=(ymax-ymin)*0.5+ymin
      write(6,*)'ymin,ymax,half intensity',ymin,ymax,half_intensity
c Do not include 0 wt points
      do j=1,npt
          if(wt(j).ne.0.)then
              if(y_temp(j).gt.half_intensity)then
                  iy_lo_fwhm=j-1
d                 write(6,*)'Lo y index for FWHM ',iy_lo_fwhm
                  js=j
                  go to 3120
              end if
          end if
      end do
c3120 do j=js,npt
 3120 do j=npt,js,-1
          if(wt(j).ne.0.)then
              if(y_temp(j).gt.half_intensity)then
                  iy_hi_fwhm=j
                  write(6,*)'Hi y index for FWHM ',iy_hi_fwhm
                  go to 3121
              end if
          end if
      end do
 3121 iy_fwhm=(iy_hi_fwhm-iy_lo_fwhm)
c$$$ calculate location of max as midpoint between two half intensity points - this allows for double peak in spectrum psf
      imax2=(iy_hi_fwhm+iy_lo_fwhm)/2
      write(6,*)'Full width half maximum is ',iy_fwhm,' centered at point ',imax2+irow_low-1
      write(6,*)'from',iy_lo_fwhm+irow_low-1,' to',iy_hi_fwhm+irow_low-1,' with min',ymin,' and max',ymax
d     write(6,*)'y_temp from iy_lo_fwhm-2 to iy_hi_fwhm+2'
d     write(6,*)(y_temp(j),j=iy_lo_fwhm-2,iy_hi_fwhm+2)
d     write(6,*)'Complete column'
d     write(6,*)(y_temp(j),j=1,npt)
      bval(4)=(y_temp(npt)-y_temp(1))/(npt-1)
      par_values(4)=bval(4)
      write(6,*)'profile slope par_values(4)= ',par_values(4)
      bval(5)=0.5*(y_temp(1)+y_temp(npt))
      par_values(5)=bval(5)
      par_values(1)=ymax-ymin
      write(6,*)'profile height and background level = ', par_values(1),par_values(5)
      mode=1
      par_values(2)=float(imax2)
      par_values(3)=float(iy_hi_fwhm-iy_lo_fwhm)*0.5
      write(6,*)'max loc and hwhm par_values(2),par_values(3)= ', par_values(2),par_values(3)
      y_mean=bval(2)
      npar=5
      ktot=0
      ipp(4)=0
      ipp(5)=0
      ii=0
      do i=1,npar
          if(i.eq.4)then
              ii=ii+1
              indice(ii)=i
              par_values(ii)=(y_temp(irow_low+1)-y_temp(irow_high-2))/float(npt-4)
              bval(4)=par_values(ii)
          else if(i.eq.5)then
              ii=ii+1
              indice(ii)=i
              par_values(ii)=0.5*(y_temp(irow_low+1)+y_temp(irow_high-2))
              bval(5)=par_values(ii)
          else if(i.eq.1)then
              ii=ii+1
              indice(ii)=i
              bval(i)=par_values(ii)
          else if(i.gt.1.and.i.le.3)then
              ii=ii+1
              indice(ii)=i
              bval(i)=par_values(ii)
          end if
          jindex(i)=0
          kindex(i)=0
          deltaa(i)=0.
      end do
      ntermm=ii
      nterm=ntermm
d     write(3,*)'Indices ',(indice(i),i=1,ntermm)
d     write(6,*)'Indices ',(indice(i),i=1,ntermm)
d     write(3,*)'Par_values ',(par_values(i),i=1,ntermm)
d     write(3,*)'Bval       ',(bval(indice(i)),i=1,ntermm)
d     write(3,*)'Param ',(bval(i),i=1,npar)
      write(6,*)'Par_values ',(par_values(i),i=1,ntermm)
      write(6,*)'Bval       ',(bval(indice(i)),i=1,ntermm)
      write(6,*)'Param ',(bval(i),i=1,npar)
      ipp(1)=4
      ipp(2)=4
      ipp(3)=4
      ipp(4)=0
      ipp(5)=0
      ipp(6)=1
      ipp(7)=1
      ipp(8)=1
      flambda=0.001
      write(6,*)ntermm,' input parameters to CURFIT'
d     write(3,*)ntermm,' input parameters to CURFIT'
      do i=1,ntermm
          write(6,*)par_values(i),indice(i),ipp(i)
d         write(3,*)par_values(i),indice(i),ipp(i)
      end do
c     write(6,*)'Before curfit for',npt,'points in column',i_col
c     write(6,902)
  902 format(1x,' x      y_temp   wt_temp ires_temp')
c     do i=1,npt
c         x_ccd=i+irow_low-1
c         write(6,903)x_ccd,y_temp(i),wt_temp(i),iresurrect_temp(i)
  903     format(1x,f4.0,2x,f8.3,6x,f2.0,5x,i2)
c     end do
c     do ii=1,npt
c         x_ccd=ii+irow_low-1
c         write(6,*)x_ccd,y_temp(ii),wt_temp(ii),iresurrect(ii)
c     end do
      write(6,*)'Fitting psf to profile'
      fit_type='Psf       '
      call curfit(x,y_temp,wt_temp,npt,ntermm,mode,par_values,deltaa,sigma_values,flambda,yfit_temp,chisqr)
      y_mean=par_values(2)
      y_error=sigma_values(2)
d     write(3,*)'i_col,ymean,yerror ',i_col,y_mean,y_error
d     write(6,*)'i_col,ymean,yerror ',i_col,y_mean,y_error
c
c     call fit_small(x,y_temp,wt,yfit,rms_temp,npt,i)
c     call smfft(x,y_temp,yfit,npt,frac,rms_temp)
c     write(6,*)(y_temp(jl),jl=1,npt)
c     write(6,*)(yfit(jl),jl=1,npt)
c
c             This option needs to be better explored, that is fitting over a larger
c             range and then taking a subset of the fit.  The technique requires:
c                  Generation of a wide spatial file
c                  Setting of wider y-limits than normal
c                  Selecting regular limits for extracting the spectrum
c             Complication:  The lamp normalizing limits the effectiveness -
c                            Not solved this one yet.
c                            Experimenting to see how far y_limits can be stretched before
c                            get divide by zero in lamp normalization.
c 
c
c Temporarily set rms_temp to chisqr for omc_ratio calculation
      rms_temp=chisqr
      write(6,*)'After fit in column',i_col,npt,' values'
      if(i_col.eq.i_col_test)write(6,*)'chisqr from curfit is',chisqr
      write(6,905)i_col
  905 format(1x,' x       y_temp     yfit_temp      omc     omc rat   wt_temp ires_temp results in column',i4)
      do i=1,npt
          x_ccd=x(i)+irow_low-1
          omc=y_temp(i)-yfit_temp(i)
          omc_ratio=omc/(times_sigma_spline*rms_temp)
          write(6,901)x_ccd,y_temp(i),yfit_temp(i),omc,omc_ratio,wt_temp(i),iresurrect_temp(i)
      end do
c     do ii=irow_low,irow_high
c         x_ccd=ii
c         write(6,*)x_ccd,y_temp(ii-irow_low+1),yfit_temp(ii-irow_low+1),wt_temp(ii-irow_low+1),iresurrect(ii-irow_low+1)
c     end do
c shift temp resultant arrays back to irow_low to irow_high
      do ii=1,npt
c maintain zero wt values of y
c         y(ii+irow_low-1)=y_temp(ii)
          yfit(ii+irow_low-1)=yfit_temp(ii)
          wt(ii+irow_low-1)=wt_temp(ii)
          iresurrect(ii+irow_low-1)=iresurrect_temp(ii)
      end do
