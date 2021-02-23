;
; SUMMARY :
;         
; 
; INPUT: 
; 
;       prefix (string) : defines the prefix of the file name
;       file0 (string)  : Formatted date  + Observation number  E .g '1045'
;
; OUTPUT:
; 
; 
; HISTORY :
;         Original file rd_chiron.pro was taken from Proff. Walter
;         Improved, make it compatible with current software. Various changes - J.Andres Lozano
;         
; E.g.    d=rd_chiron(l,/xfile) 
;

function rd_chiron,file0,sp,head, $
  linearize=linearize,order=order,lambda=lambda, $
  all=all,stats=stats,fftsm0=fftsm0,smoothx=smoothx,ynoz=ynoz,oplot=oplot, $
  prefix=prefix,show=show,gauss=gauss,helpme=helpme,stp=stp
  procname='Rd_Chiron()'

  if ~keyword_set(xfile) then prefix='achi'
  
  if keyword_set(file0) then begin
    if not ifstring(file0) then begin
      l=getfilelist(pref+'*.'+strtrim(file0,2)+'.fits',nl)
      if nl eq 1 then file0=l[0]
      file0=strmid(file0,0,strlen(file0)-5)
    endif
  endif
  
  if ifstring(file0) then begin
    if strlen(file0) eq 4 then begin
      l=getfilelist(pref+'*.'+file0+'.fits',nl)
      if nl eq 1 then file0=l[0]
      file0=strmid(file0,0,strlen(file0)-5)
    endif
  endif
  
  if not ifstring(file0) then helpme=1
  if keyword_set(helpme) then begin
      print,' '
      print,'* ',procname,' - read extracted Chiron data file'
      print,'* Calling Sequence: z=',procname,'(file,sp,head)'
      print,'*    Z   : output structure'
      print,'*    FILE: input file name'
      print,'*    SP:   output 2 x nx x nord fits array'
      print,'*    HEAD: output fits header'
      print,'* '
      print,'* KEYWORDS:'
      print,'*    ALL:   linearize,order,lambda'
      print,'*    RAW:   if fluxed, return raw data'
      print,'*    XFILE: set to read xchi file, default=achi'
      print,'*    stats,fftsm0,smooth,ynoz,oplot'
      print,' '
      return,-1
  endif
  
  file=file0
  k=strpos(file,'.fits')
  if k gt 0 then file=strmid(file,0,k)    ;strip extension
  if not ffile(file+'.fits') then begin                              ; missing  ffile.pro
    print,procname+': File ',file+'.fits not found - returning'
    if keyword_set(stp) then stop,procname+'>>>'
    return,-1
  endif



  ;*****************************************
  ; Reading file 
  ;*****************************************
  sp=readfits(file+'.fits',head,/silent)
  nn=(size(sp))[1]
  w=reform(sp[0,*,*])
  s=reform(sp[1,*,*])
  if nn ge 3 then snr=reform(sp[2,*,*]) else snr=sqrt(s>1.e-6)
  ;eb=s/snr
  if nn ge 4 then dq=reform(sp[3,*,*])
  if nn ge 5 then gd=reform(sp[4,*,*])   ;gaussian extraction
  fcr=sxpar(head,'fcalrec')
  gcr=sxpar(head,'grec')
  if (fcr gt 0) then fs=reform(sp[fcr-1,*,*])
  nx=(size(w))[1]
  nord=(size(w))[2]
  narr=n_elements(w)
  if keyword_set(all) then order=indgen(nord)

  ss=s & ws=w

  if keyword_set(linearize) then for i=0,nord-1 do begin
    ww=w[*,i]
    wr=ww[nx-1]+(ww[nx-1]-ww[nx-2])-ww[0]
    apb=wr/nx
    wn=ww[0]+apb*findgen(nx)
    f=interpol(s[*,i],ww,wn)
    if (fcr gt 0) then begin
      ff=interpol(fs[*,i],ww,wn) & fs[*,i]=ff
    endif
    snr=interpol(snr,ww,wn)
    eb=interpol(eb,ww,wn)
    s[*,i]=f & w[*,i]=wn
  endfor

  case 1 of
    (fcr gt 0) and (gcr gt 0): z={w:w,s:s,e:snr,dq:dq,g:gd,f:fs,head:head}
    fcr gt 0: z={w:w,s:s,e:snr,dq:dq,f:fs,head:head}
    gcr gt 0: z={w:w,s:s,e:snr,dq:dq,g:gd,head:head}
    n_elements(dq) eq narr: z={w:w,s:s,e:snr,dq:dq,head:head}
    ;   nn ge 5: z={w:w,s:s,e:eb,dq:dq,g:gd,head:head}
    else: z={w:w,s:s,e:snr,head:head}
  endcase
  ;
  time=sxpar(head,'exptime')
  if keyword_set(stats) then print,' Total exposure time = ',time
  obj=sxpar(head,'object')
  utd=sxpar(head,'utshut')

  nord=n_elements(order)

  if keyword_set(show) then begin
    setxy
    iord=52
    while (iord ge 0) and (iord le nord-1) do begin
      m=138-iord
      if (nn ge 5) and keyword_set(gauss) then s=z.g else s=z.s
      plot,z.w,s,xtit='!6Angstroms',tit='!6'+obj+' m='+strtrim(m,2)+' order='+strtrim(iord,2)
      blowup,-1
      case 1 of
        zerr eq 60: iord=iord-1
        zerr eq 62: iord=iord+1
        ;      (zerr eq 70) or (zerr eq 102): begin
        ;         ff=fflat[*,iord]
        ;         frat=total(dx.s[200:800,iord])/total(fflat[200:800,iord])
        ;         oplot,col=2,dx.w[*,iord],fflat[*,iord]*frat
        ;         end
        (zerr eq 81) or (zerr eq 113): iord=-1
        (zerr eq 90) or (zerr eq 122): stop,procname+'>>>'
        else: iord=iord+1
      endcase
    endwhile
    setxy
  endif

  if nord gt 1 then print,' hit any key to show next order'
  if nord ge 1 then for ip=0,nord-1 do begin
    xsty=0
    ww=w[2:nx-3,order[ip]] & ff=s[2:nx-3,order[ip]] & ee=eb[2:nx-3,order[ip]]  ;trim edges
    l1=min(ww) & l2=max(ww)
    if n_elements(lambda) eq 2 then setxy,lambda[0]>l1,lambda[1]<l2 else begin
      if (!x.crange[0] ge l2) or (!x.crange[1] le l1) then setxy
    endelse
    if n_elements(lambda) eq 2 or (!x.crange[0] gt 1000.) then xsty=1
    if keyword_set(smoothx) then begin
      ff=smooth(ff,smoothx>3) & ee=ee/sqrt(smoothx>3)
    endif
    if keyword_set(fftsm0) then begin
      fs0=fftsm0
      fftsm,ff,1,fs0
      fs0=fftsm0
      fftsm,ee,1,fs0
    endif
    if keyword_set(oplot) then oplot,ww,ff/time,col=oplot else plot,ww,ff/time,ynoz=ynoz,xtit='!6Angstroms',ytit='!6Counts/sec',charsize=2,xsty=xsty,tit='!6'+obj+' '+utd+' ord: '+strtrim(order[ip],2)
    if keyword_set(stats) then begin
      fs=median(ff,31)
      r=fs(sort(fs)) & sf=median((reverse(r))[0:100])
      print,'maximum CR = ',max(sf/time),'  Max counts = ',max(sf)
    endif
    if nord gt 1 then begin
      cursor,x,y & wait,0.2
      case !mouse.button of
        2: goto,retn
        4: ip=ip-2
        else:
      endcase
    endif
    print,nord,ip,order(ip)
  endfor
  ;
  retn:
  if keyword_set(stp) then stop,procname+'>>>'
  return,z
end
