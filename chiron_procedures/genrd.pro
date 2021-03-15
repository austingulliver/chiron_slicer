;*******************************************************************
pro genrd,lu0,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17, $
  i18,i19,i20,noclose=noclose,skiplines=skiplines,silent=silent,ncol=ncol, $
  flag=flag,last=last,buffer=buffer,maxlines=maxlines,endline=endline, $
  nulll=nulll,bad=bad,cut=cut,helpme=helpme,stp=stp
  procname='GenRd'
  ;
  if n_params() eq 0 then helpme=1
  if keyword_set(helpme) then begin
    print,' '
    print,'* ',procname,' - reads text files into arrays'
    print,'* calling sequence: ',procname,',lu,a,b,c ...'
    print,'*    lu: LUN or file name'
    print,'*    a,b,c: name of predefined variables, up to 20 allowed'
    print,'* '
    print,'* KEYWORDS:'
    print,'*    BAD:       string marking first character in line to be skipped *'
    print,'*    BUFFER:    size of array to predefine'
    print,'*    CUT:       string marking last line to read *'
    print,'*    FLAG:      -1 if file not found'
    print,'*    MAXLINES:  maximum number of lines to read in'
    print,'*    NCOL:      number of columns for 2D array'
    print,'*    NOCLOSE:   if not set, LU will be closed'
    print,'*    NULL:      set to include null lines *'
    print,'*    SKIPLINES: number of lines to skip at start, 0 to skip 1'
    print,'*    silent,last'
    print,'*    * - applicable to single-element string arrays only'
    print,' '
    return
  endif

  if n_elements(maxlines) eq 0 then maxlines=0
  if maxlines lt 1 then maxlines=0 else maxlines=maxlines-1
  ninp=n_params()-1
  if (ninp eq 1) and (n_elements(i1) gt 1) then begin
    doarr=1 & nn=n_elements(i1)
  endif else doarr=0
  on_ioerror,misread
  lu=lu0
  if ifstring(lu) then begin
    file=lu
    
    if  FILE_TEST(file)  eq 0 then begin
      print,' File ',file,' not found - returning'
      flag=-1
      if keyword_set(stp) then stop,'GENRD>>>'
      return
    end
    openr,lu,file,/get_lun
  endif else file=0
  if keyword_set(last) then begin
    sl=strlen(last)
    z=''
    if n_elements(skiplines) eq 1 then begin
      if skiplines ge 0 then for i=0,skiplines do readf,lu,z
    endif
    zz=''
    while not eof(lu) do begin
      readf,lu,z & zz=[zz,z]
    endwhile
    maxlines=n_elements(zz)
    close,lu & openr,lu,file
  endif   ;last
  z=''
  if n_elements(skiplines) eq 1 then begin
    if skiplines ge 0 then for i=0,skiplines do readf,lu,z
  endif
  if keyword_set(endline) then lend=strlen(endline)
  case 1 of
    keyword_set(ncol): begin
      a1=dblarr(ncol>1)
      i1=a1
      readf,lu,i1
      if maxlines gt 0 then for il=0,maxlines-1 do begin
        readf,lu,a1
        i1=[[i1],[a1]]
      endfor else while not eof(lu) do begin
        readf,lu,a1
        i1=[[i1],[a1]]
      endwhile
    end
    ninp eq 1: begin
      a1=i1
      readf,lu,i1
      case 1 of
        keyword_set(buffer): begin
          i1=replicate(i1,buffer>1)
          ii=1L
          while not eof(lu) do begin
            readf,lu,a1
            i1[ii]=a1
            if ii ge (buffer-1) then goto,obuf
            ii=ii+1L
          endwhile
          obuf:
          i1=i1[0:ii]
        end
        maxlines gt 0: for il=0L,maxlines-1L do begin
          readf,lu,a1
          i1=[[i1],[a1]]
        endfor
        else: while not eof(lu) do begin
          readf,lu,a1
          if keyword_set(endline) then if strmid(a1,0,lend) eq endline then goto,bail
          i1=[i1,a1]
        endwhile
      endcase
      bail:
      if ifstring(cut) then begin
        nc=strlen(cut)
        k=where(strmid(i1,0,nc) eq cut,nk)
        if nk gt 0 then i1=i1[0:k[0]-1]
      endif
      if ifstring(bad) then begin
        nb=strlen(bad)
        k=where(strmid(i1,0,nb) ne bad,nk)
        if nk gt 0 then i1=i1[k]
      endif
      if ~keyword_set(nulll) then begin
        sl=strlen(strtrim(i1,2))
        kg=where(sl gt 0,nkg)
        if nkg gt 0 then i1=i1[kg]
      endif
    end    ;ninp=1
    ninp eq 2: begin
      a1=i1 & a2=i2
      readf,lu,i1,i2
      while not eof(lu) do begin
        readf,lu,a1,a2
        i1=[i1,a1]
        i2=[i2,a2]
      endwhile
    end
    ninp eq 3: begin
      a1=i1 & a2=i2 & a3=i3
      readf,lu,i1,i2,i3
      while not eof(lu) do begin
        readf,lu,a1,a2,a3
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
      endwhile
    end
    ninp eq 4: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4
      readf,lu,i1,i2,i3,i4
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
      endwhile
    end
    ninp eq 5: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5
      readf,lu,i1,i2,i3,i4,i5
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
      endwhile
    end
    ninp eq 6: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6
      readf,lu,i1,i2,i3,i4,i5,i6
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
      endwhile
    end
    ninp eq 7: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      readf,lu,i1,i2,i3,i4,i5,i6,i7
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
      endwhile
    end
    ninp eq 8: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a8=i8
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
      endwhile
    end
    ninp eq 9: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a8=i8 & a9=i9
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
      endwhile
    end
    ninp eq 10: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a10=i10 & a8=i8 & a9=i9
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
      endwhile
    end
    ninp eq 11: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a10=i10 & a11=i11 & a8=i8 & a9=i9
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
      endwhile
    end
    ninp eq 12: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
      endwhile
    end
    ninp eq 13: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
      endwhile
    end
    ninp eq 14: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
      endwhile
    end
    ninp eq 15: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7 & a15=i15
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
        i15=[i15,a15]
      endwhile
    end
    ninp eq 16: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7 & a15=i15
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      a16=i16
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
        i15=[i15,a15]
        i16=[i16,a16]
      endwhile
    end
    ninp eq 17: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7 & a15=i15
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      a16=i16 & a17=i17
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
        i15=[i15,a15]
        i16=[i16,a16]
        i17=[i17,a17]
      endwhile
    end
    ninp eq 18: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7 & a15=i15
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      a16=i16 & a17=i17 & a18=i18
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
        i15=[i15,a15]
        i16=[i16,a16]
        i17=[i17,a17]
        i18=[i18,a18]
      endwhile
    end
    ninp eq 19: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7 & a15=i15
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      a16=i16 & a17=i17 & a18=i18 & a19=i19
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,i19
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
        i15=[i15,a15]
        i16=[i16,a16]
        i17=[i17,a17]
        i18=[i18,a18]
        i19=[i19,a19]
      endwhile
    end
    ninp eq 20: begin
      a1=i1 & a2=i2 & a3=i3 & a4=i4 & a5=i5 & a6=i6 & a7=i7 & a15=i15
      a10=i10 & a12=i12 & a11=i11 & a8=i8 & a9=i9 & a13=i13 & a14=i14
      a16=i16 & a17=i17 & a18=i18 & a19=i19 & a20=i20
      readf,lu,i1,i2,i3,i4,i5,i6,i7,i8,i9,i10,i11,i12,i13,i14,i15,i16,i17,i18,i19,i20
      while not eof(lu) do begin
        readf,lu,a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20
        i1=[i1,a1]
        i2=[i2,a2]
        i3=[i3,a3]
        i4=[i4,a4]
        i5=[i5,a5]
        i6=[i6,a6]
        i7=[i7,a7]
        i8=[i8,a8]
        i9=[i9,a9]
        i10=[i10,a10]
        i11=[i11,a11]
        i12=[i12,a12]
        i13=[i13,a13]
        i14=[i14,a14]
        i15=[i15,a15]
        i16=[i16,a16]
        i17=[i17,a17]
        i18=[i18,a18]
        i19=[i19,a19]
        i20=[i20,a20]
      endwhile
    end
    else: print,'ERROR in genrd: ',ninp,' inputs. 1-20 allowed'
  endcase
  ;
  goto,done
  ;
  misread:
  s=n_elements(i1)
  if not keyword_Set(silent) then $
    print,' GENRD returning after reading',s,' records.'
  ;
  done:
  if doarr then begin
    np=n_elements(i1)/nn
    i1=reform(i1,nn,np)
  endif
  if not keyword_set(noclose) then close,lu
  if ifstring(file) then free_lun,lu
  flag=n_elements(i1)
  if keyword_set(stp) then stop,'GENRD>>>'
  return
end
