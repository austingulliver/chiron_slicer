;
; SUMMARY :
;          It splices the extracted orders together
; 
; INPUT:
;
; OUTPUT:
;


function chi_splice,date,file,k11=k11,trim=trim,scale=scale, $
  weight=weight,sum=sum,cut=cut,nosave=nosave,savfile=savfile, $
  ds=ds,z=z,plt=plt, examine=examine,helpme=helpme,stp=stp
 ; common comxy,xcur,ycur,zerr,xx1,xx2,xx3
 ; common ch_calfiles,fcalfile,zfcal,zfcor,caldate
 
  procname='Ch_Splice'

  if keyword_set(helpme) then begin
    print,' '
    print,'* ',procname,' - splice fluxed Chiron data'
    print,'* calling sequence: ',procname,',date,file'
    print,'*    INPUT: date and file, or just file number, or full name'
    print,'* '
    print,'* KEYWORDS'
    print,'*    CUT, SUM, WEIGHT'
    print,'*    K11:   def=190'
    print,'*    SCALE: '
    print,'*    TRIM:  def=[0,0]'
    return,0
  endif
  
  ;*****************************************
  ; Initial Variables 
  ;*****************************************
  
  
  chdata= getenv('CHIRON_PATH')  ;This must the absolute path where the directory chiron was placed
                                 ; E.g.  SETENV, 'CHIRON_PATH=.......\chiron'
                                 
  IF strlen(chdata) le 1 then stop,  'CHI_SPLICE: Set Env. variable CHIRON_PATH before continue. E.g SETENV, CHIRON_PATH=.......\chiron '
  if keyword_set(examine) then plt=1
  if keyword_set(plt) then gc,13
  

  if not keyword_set(k11) then k11=190

  case n_elements(trim) of
    150:
    0: trim=intarr(2,75)
    1: trim=intarr(2,75)+trim
    else: begin
      tr=trim[0,1]
      trim=intarr(2,75)
      trim[0,*]=tr[0] & trim[1,*]=tr[1]
    end
  endcase

  trimfile=chdata+'/tous/mir7/utils/chi_deftrims.dat'
  
  i=0 & t1=0 & t2=0
  genrd,trimfile,i,t1,t2
  trims=intarr(2,75)
  trims[0,*]=transpose(t1)+trim[0,*]
  trims[1,*]=transpose(t2)+trim[1,*]

  mode='weight'
  case 1 of
    keyword_set(sum): mode='sum'
    keyword_set(cut): mode='cut'
    else: weight=1
  endcase
  if not keyword_set(cut) then cut=0
  
  
  ;*****************************************
  ; Collecting files 
  ;*****************************************
  
  case 1 of
    n_params() eq 2: l=chdata+'/'+date+'/xchi'+date+'.'+strtrim(file,2)
    n_params() eq 1: if strlen(date) gt 12 then l=date else begin
      l=getfilelist('xchi*.'+strtrim(date,2)+'*.fits',nl)
      if nl gt 1 then begin
        igo=0
        for i=0,nl-1 do print,i,'  ',l[i]
        read,prompt='Please enter index of file: ',igo
        igo=(igo>0)<(nl-1)
        l=l[igo]
      endif else l=l[0]
      print,' Selected file: ',l
    end
    else: begin
      l=getfilelist('xchi*.fits',nl)
      if nl gt 1 then begin
        igo=0
        for i=0,nl-1 do print,i,'  ',l[i]
        read,prompt='Please enter index of file: ',igo
        igo=(igo>0)<(nl-1)
        l=l[igo]
      endif else l=l[0]
      print,' Selected file: ',l
    end
  endcase

  print,procname,': reading ',l
  d=rd_chiron(l,/xfile)
  k=where('F' eq strupcase(tag_names(d)),nk)
  if nk eq 0 then begin
    print,' No fluxed data found - run ch_xupdate,/flux'
    if keyword_set(stp) then stop,procname+'>>>'
    return,0
  endif
  
  
  ylen=(size(d.w))[1]
  nord=(size(d.w))[2]
  if ylen eq 4112 then mode='slicer' else mode='fiber'
  if mode eq 'slicer' then ysf=4 else ysf=1
  k11=k11*ysf

  exptime=sxpar(d.head,'exptime')
  ksplice=-1
  dw=median(d.w[k11+1:*,0]-d.w[k11:*,0])    ;median A/pix in order 0
  w0=d.w[trims[0,0]*ysf,0] & w1=d.w[ylen-1,nord-1]
  np=1+long((w1-w0)/dw)
  print,procname,': vector length = ',np
  w=w0+dw*dindgen(np)
  f=dblarr(np) & e=f+999999.
  n=intarr(np)
  dq=intarr(np)
  if keyword_set(examine) then print,' Hit any key to continue following plot (zq).'

  for i=0,nord-1 do begin
    k=trims[0,i]*ysf+indgen(ylen-trims[0,i]*ysf-trims[1,i]*ysf)
    s=interpol(d.f[k,i],d.w[k,i],w)
    eb=abs(d.f[k,i]/(d.e[k,i]>1.e-6))   ;S/N vector -> error bar
    eb=interpol(eb,d.w[k,i],w)
    dq1=interpol(d.dq[k,i],d.w[k,i],w)
    nn=intarr(np)+1
    k1=(where(w ge d.w[k[0],i]))[0]
    k2=(where(w ge max(d.w[k,i])))[0]
    if k2 le np-1 then begin
      s[k2:*]=0.
      eb[k2:*]=0.
      nn[k2:*]=0
      dq1[k2:*]=0
    endif
    if i eq 0 then begin
      f=s & e[k1:k2]=(eb*eb)[k1:k2] & n=n+nn & dq=dq1
    endif else begin
      s[0:k1]=0. & eb[0:k1]=0. & nn[0:k1]=0& dq1[0:k1]=0
      kv1=max(where(f gt 0.)) & kv2=(where(s gt 0.))[0]
      nkov=kv1+1-kv2
      if nkov gt 0 then kov=kv2+lindgen(nkov) else kov=-1
      if keyword_set(weight) and (nkov le 0) then sum=1   ;change mode
      kov1=kov[0]
      kov2=max(kov)+1
      ks1=w[kov1] & ks2=w[kov2]    ;splices
      if keyword_set(scale) then begin
        if nkov gt 3 then rat=median(f[kov]/s[kov]) else begin
          ko1=where(f gt 0.) & ko1=max(ko1)-26+indgen(25)
          ko2=where(s gt 0.) & ko2=ko2[0]+indgen(25)
          rat=median(f[ko1])/median(s[ko2])
        endelse
        print,i,' ratio = ',rat
        s=s*rat & eb=eb*rat
      endif

      case 1 of
        keyword_set(sum): begin
          f=f+s & e=e+eb*eb & n=n+nn & dq=dq+dq1
          if nkov gt 0 then ksplice=[ksplice,ks1,ks2]  ;splice points
        end
        keyword_set(cut): begin   ;cut by S/N
          kc1=where((eb gt 0.) and (eb lt sqrt(e)),nkc1); new data
          kc1=kc1[0]+indgen(max(kc1)-kc1[0]+1)
          if keyword_set(plt) then begin
            setxy,d.w[0,i-1],max(d.w[*,i])
            plot,w,f,tit='!6order: '+strtrim(i,2)
          endif
          f[kc1]=s[kc1]
          e[kc1]=e[kc1]+eb[kc1]*eb[kc1]
          if keyword_set(plt) then begin
            oplot,w,f,col=2
            oplot,w[kc1],s[kc1],col=5
            if keyword_set(examine) then begin
              blowup,-1
              if (zerr eq 90) or (zerr eq 122) then stop,procname+'>>>'
              if (zerr eq 81) or (zerr eq 113) then return,-1
            endif
          endif
          ks=[ks,w[kc1[0]]]    ;splice point
          dq=dq+dq1
        end
        else: begin ;weighting  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
          ff=f
          wt0=(1./sqrt(e[kov])+1./eb[kov])
          npo=4<(n_elements(kov)-1)
          if npo gt 1 then begin
            cc1=robust_poly_fit(kov-kov[0],1./sqrt(e[kov]),npo,efit)
            cc2=robust_poly_fit(kov-kov[0],1./eb[kov],npo,ebfit)
            wt=efit+ebfit
          endif else begin
            wt=wt0
            efit=1./sqrt(e[kov]) & ebfit=1./eb[kov]
          endelse
          kz1=where(f[kov] le 0.0,nkz1)
          kz2=where(s[kov] le 0.0,nkz2)
          f[kov]=((f[kov]*efit+s[kov]*ebfit)/wt)>0.
          if nkz1 gt 0 then f[kov[kz1]]=s[kov[kz1]]>0.
          if nkz2 gt 0 then f[kov[kz2]]=ff[kov[kz2]]>0.
          e[kov]=e[kov]+eb[kov]*eb[kov]
          if k2-1-max(kov) le 0 then begin
            printc,col='red',/hi,procname+': Error in wavecal solution for order '+strtrim(i,2)
            printc,col='red',/hi,'     Re-run ch_reduce with a different caldate'
            print,'FCalFile=',fcalfile,' caldate= ',caldate
            retall
          endif
          kn=max(kov)+1+indgen(k2-1-max(kov))   ;region w/o overlap
          f[kn]=s[kn] & e[kn]=eb[kn]*eb[kn]
          if keyword_set(plt) then begin
            setxy,d.w[0,i-1],d.w[ylen-1,i]
            plot,w,f,tit='!6 Order: '+strtrim(i,2)
            oplot,w,ff,col=1 & oplot,w,s,col=5
            oplot,w,f
            if keyword_set(examine) then begin
              blowup,-1
              if (zerr eq 90) or (zerr eq 122) then stop,procname+'>>>'
              if (zerr eq 81) or (zerr eq 113) then return,-1
            endif
          endif
          dq=dq+dq1
          if nkov gt 0 then ksplice=[ksplice,ks1,ks2]  ;splice points
        end
      endcase
    endelse  ; order>0
  endfor

  if keyword_set(sum) then begin
    f=f/(nn>1)/exptime
    e=e/(nn>1)/exptime
  endif

  if keyword_set(plt) then begin
    setxy
    klx=strpos(l,'xchi') & if klx gt 0 then lx=strmid(l,klx) else lx=l
    plot,w,f,xtit='!6Angstroms',ytit=ytit(0),tit='!6'+lx+'  '+strtrim(sxpar(d.head,'object'),2)
  endif   ;plt

  head=d.head
  if ifstring(zfcal) then sxaddpar,head,'FluxCal',zfcal
  if ifstring(zfcor) then sxaddpar,head,'FluxCor',zfcor
  sxaddpar,head,'splice',mode

  e=sqrt(e)
  if n_elements(ksplice) gt 1 then ksplice=ksplice[1:*]
  if n_elements(ds) eq 0 then ds=-1
  z={w:d.w,s:d.s,e:d.e,f:d.f,h:head,w1:w,f1:f,e1:e,dq:dq,splice:ksplice,sky:0.,ds:ds}

  if not keyword_set(nosave) then begin
    ll=l
    k=strpos(ll,'.fits') & if k ne -1 then ll=strmid(ll,0,k)
    s=strlen(chdata)
    if strmid(ll,0,s) eq chdata then begin    ;full name passed
      s=strlen(chdata)+8
      ll=strmid(ll,s)
    endif
    if strmid(ll,0,2) eq 'x2' then dx=1 else dx=0
    if strmid(ll,0,2) eq 'x2' then zx='_c' else zx=''
    ll=strmid(ll,4+dx,6)+'.'+strmid(ll,11+dx,4)
    if ifstring(savfile) then savf=savfile else savf=strcompress(strtrim(sxpar(head,'object'),2),/remove_all)+'_'+ll+zx
    save,z,file=savf+'.sav'
    print,' Spliced data written to ',savf+'.sav'
    savfile=savf
  endif

  if keyword_set(stp) then stop,procname+'>>>'
  return,z
end
