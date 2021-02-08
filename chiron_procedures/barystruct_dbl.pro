;+
;
;
;  PURPOSE: Has the same purpose as chi_barystruct.pro but can be applied for various 
;  observatories.
;
;
;  MODIFICATION HISTORY:
;        J. Andres Lozano 2021.01.31 : Added comments + trying to find meaning
;
;-
pro barystruct_dbl,observatory=observatory
    
    ;Identifying the observatory
    if strlowcase(observatory) eq 'magellan' then begin
        path='/mir5/bary/'
        asciifile='/mir5/bary/mbcvel.ascii'
        baryfile='mbcvel.dat'
    endif
    if strlowcase(observatory) eq 'keck' then begin
        path='/mir3/bary/'
        asciifile='/mir3/bary/kbcvel.ascii'
        baryfile='kbcvel.dat'
    endif
    if strlowcase(observatory) eq 'ctio' then begin
        path='/tous/mir7/bary/'
        asciifile='/tous/mir7/bary/qbcvel.ascii'
        baryfile='qbcvel.dat'
    endif
    if strlowcase(observatory) eq 'lick' then begin
        path='/mir1/bary/'
        asciifile='/mir1/bary/bcvel.ascii'
        baryfile='bcvel.dat'
    endif
    if strlowcase(observatory) eq 'aat' then begin
        path='/mir2/bary/'
        asciifile='/mir2/bary/ubcvel.ascii'
        baryfile='ubcvel.dat'
    endif
    
    
    ; Open File + count lines
    ;--------------------------------------------------------------
    openr,1,asciifile    
    line=' '
    count=0L    
    while ~ eof(1) do begin ;Count number of lines in file
    	readf,1,line
    	count=count+1L
    close,1
    
    
    ; Open File + count lines
    ;--------------------------------------------------------------
    
    bcat={obsnm:'?', objnm:'?', bc:0.0, jd:0.0d, ha:0.0, obtype:'?'}
    bcat=replicate(bcat,count-1L)
    
    openr,1,asciifile
    readf,1,line  		  ; throw away the first line
    for i=0,30000l-1 do begin  ;because first line dropped
      	readf,1,line
      	bcat(i).obsnm=getwrd(line,0)
      	bcat(i).objnm=getwrd(line,1)
      	bcat(i).bc=float(getwrd(line,2))
      	bcat(i).jd=double(getwrd(line,3))
      	bcat(i).ha=float(getwrd(line,4))
      	bcat(i).obtype=getwrd(line,5)
    end
    
    add1=30000L
    for i=0,add1-2L do begin  ;because first line dropped
      	readf,1,line
      	bcat(i+30000l).obsnm=getwrd(line,0)
      	bcat(i+30000l).objnm=getwrd(line,1)
      	bcat(i+30000l).bc=float(getwrd(line,2))
      	bcat(i+30000l).jd=double(getwrd(line,3))
      	bcat(i+30000l).ha=float(getwrd(line,4))
      	bcat(i+30000l).obtype=getwrd(line,5)
    endfor
    
    addnum=count-(30000l+add1)  ; less than 30000l
    
    for i=0,addnum-2L do begin   ;because first line dropped
      	readf,1,line
      	bcat(i+60000l).obsnm=getwrd(line,0)
      	bcat(i+60000l).objnm=getwrd(line,1)
      	bcat(i+60000l).bc=double(getwrd(line,2))
      	bcat(i+60000l).jd=float(getwrd(line,3))
      	bcat(i+60000l).ha=float(getwrd(line,4))
      	bcat(i+60000l).obtype=getwrd(line,5)
    endfor 
    
    
    close,1
    
    save,bcat,file=path+baryfile
END
