
function get_file_list,name,nf,noversion=noversion,noext=noext, $
    full=full,helpme=helpme,direc=direc,nodirec=nodirec
    ; get a list of files  and put names in filestr
    if keyword_set(helpme) then begin
        print,' '
        print,'* GETFILELIST = return string array of file names'
        print,'* calling sequence: list=GETFILELIST(name,nf)'
        print,'* note: this is a VMS routine - under UNIX this calls FINDFILE'
        print,'*    NAME: file identifier (including wild cards)'
        print,'*    NF: number of files'
        print,'* '
        print,'* KEYWORDS:'
        print,'*    DIREC:     directory to search'
        print,'*    NODIREC:   if set, do not prepend directory name'
        print,'*    NOEXT:     do not return extension'
        if !version.os eq 'vms' then print,'*    NOVERSION: do not return version number'
        print,' '
        return,''
    end
    if n_elements(direc) eq 0 then direc='' else direc=direc+'/'               ;'[]'
    if n_elements(dir) eq 0 then dir=direc
    if n_params() lt 1 then name='*'
    ;
    if !version.os ne 'vms' then begin
        filestr=file_search(direc+name)
        nf=n_elements(filestr)
        if (nf eq 1) and (strlen(filestr[0]) eq 0) then nf=0
        if nf eq 0 then return,''
        if keyword_set(noext) then for ii=0,nf-1 do begin
          k=strpos(filestr[ii],'.',/reverse_search)
          if k ne -1 then filestr[ii]=strtrim(strmid(filestr[ii],0,k),2)
        endfor
        if keyword_set(nodirec) then for ii=0,nf-1 do begin
          k=strpos(filestr[ii],'/',/reverse_search)
          if k ne -1 then filestr[ii]=strtrim(strmid(filestr[ii],k+1),2)
        endfor
        ;
        return,filestr
    endif           ;not VMS
    ; VMS version - slower, but prettier output
    tmpfl='ftemp.tmp'
    filestr=strarr(24,400)
    k=strpos(name,'.')
    kall=strpos(name,'*')
    z='dir/size/out='+tmpfl+' '+direc+name
    if n_elements(dir) eq 1 then cd,dir,current=current
    spawn,z
    get_lun,lu
    openr,lu,tmpfl
    z=''
    ii=0         ;counter
    on_ioerror,none
    for i=0,2 do readf,lu,z
    on_ioerror,null
    while not eof(lu) do begin
        readf,lu,z
        kver=strpos(z,';')
        if kver ne -1 then begin
            k=strpos(z,' ')   ;position of first blank following name
            if keyword_set(noversion) then k=strpos(z,';')
            if keyword_set(noext) then k=strpos(z,'.')
            if k eq -1 then k=strlen(z)
            s=strtrim(strmid(z,0,k),2)
            filestr[ii]=s
            ii=ii+1
        endif
    endwhile
    close,lu
    free_lun,lu
    filestr=filestr[0:ii-1]
    z='delete/noconfirm '+tmpfl+';*'
    spawn,z,/nowait
    if n_elements(dir) eq 1 then cd,current
    nf=n_elements(filestr)
    return,filestr
    none:    ;no files match
    free_lun,lu
    nf=0
    return,''
end
