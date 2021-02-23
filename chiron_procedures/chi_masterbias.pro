;+
;
;  NAME: 
;     chi_medianbias
;
;  PURPOSE: 
;    To create and store median/mean  bias frames for the various modes for 
;	 bias subtraction
;
;  CATEGORY:
;      CHIRON
;
;
;  INPUTS:
;
;  OPTIONAL INPUTS:
;
;  OUTPUTS:
;
;  KEYWORD PARAMETERS:
;    
;  EXAMPLE:
;      chi_masterbias, redpar = redpar, log = log, /bin31, /normal
;
;  MODIFICATION HISTORY:
;        c. Matt Giguere 2012.04.17 04:09:19 PM
;           Jorge Lozano 2020.10.13 04:09:19 PM : mean master bias option created  
;
;-
pro chi_masterbias, $
help = help, $
postplot = postplot, $
redpar = redpar, $
bin11 = bin11, $
bin31 = bin31, $
bin44 = bin44, $
normal = normal, $
fast = fast, $
log = log, $
bobsmed = bobsmed, $
master_bias = master_bias ; Takes 2 options "  'median' or 'mean'


if keyword_set(bin31) then binsz='31'
if keyword_set(bin11) then binsz='11'
if keyword_set(bin44) then binsz='44'
if keyword_set(normal) then rdspd = 'normal'
if keyword_set(fast) then rdspd = 'fast'


binning = strtrim(string(redpar.binnings[redpar.mode]),2)



;#####################################################
;##  Define bias frames to be used.
;#####################################################


bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '3 1' and strt(log.speedmod) eq 'normal', bobsct)   ; Default 

if keyword_set(bin11) and keyword_set(normal) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '1 1' and strt(log.speedmod) eq 'normal', bobsct) ; Overwritten
endif

if keyword_set(bin11) and keyword_set(fast) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '1 1' and strt(log.speedmod) eq 'fast', bobsct)    ; Overwritten
endif

if keyword_set(bin31) and keyword_set(fast) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '3 1' and strt(log.speedmod) eq 'fast', bobsct)   ; Overwritten 
endif

if keyword_set(bin44) and keyword_set(normal) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '4 4' and strt(log.speedmod) eq 'normal', bobsct) ; Overwritten 
endif




;#####################################################
;##   Construct Master Bias 
;#####################################################


if bobsct ge 2 then begin
    print, 'CHI_MEDIANBIAS: Creating Master Bias, please wait ..... '
    print, 'CHI_MEDIANBIAS: Number of bias frames used : ' + strtrim(string(bobsct),2)
    print, 'CHI_MEDIANBIAS: Binning of frames          : ' + binning
    print, 'CHI_MEDIANBIAS: Speed Mode                 : ' + rdspd
  
      
    ;bcube = dblarr(long(log[bobs[0]].naxis1), long(log[bobs[0]].naxis2), bobsct)    
    bcube = dblarr(long(log.naxis1[bobs[0]]), long(log.naxis2[bobs[0]]), bobsct)  ; modfied by JL to make it compatibla with creation of "createlogstruc"
    
    for i=0, bobsct-1 do begin
      
        ;biasim = double(readfits(log[bobs[i]].filename, hd)) ; by JL
        biasim = double(readfits(log.filename[bobs[i]], hd))
        geom = chip_geometry(hdr=hd)
        
        ;1. subtract median value from upper LEFT QUADRANT (both image and overscan region):
        ;  --------------------------------------------------------------------------------
        idx = [0L, geom.bias_full.upleft[1], geom.image_trim.upleft[2], geom.image_trim.upleft[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.upleft[0]:geom.bias_trim.upleft[1], geom.bias_trim.upleft[2]:geom.bias_trim.upleft[3]])
        
        ;>>2. now do the same for the UPPER RIGHT quadrant:
        ;  -----------------------------------------------
        ;idx = [geom.bias_full.upright[0], log[bobs[0]].naxis1-1, geom.image_trim.upright[2], geom.image_trim.upright[3]] ;BEFORE, updated by JL
        idx = [geom.bias_full.upright[0], log.naxis1[bobs[0]]-1, geom.image_trim.upright[2], geom.image_trim.upright[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.upright[0]:geom.bias_trim.upright[1], geom.bias_trim.upright[2]:geom.bias_trim.upright[3]])
        
        ;>> 3. and the BOTTOM LEFT quadrant:
        ;----------------------------------
        idx = [0L, geom.bias_full.botleft[1], geom.image_trim.botleft[2], geom.image_trim.botleft[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.botleft[0]:geom.bias_trim.botleft[1], geom.bias_trim.botleft[2]:geom.bias_trim.botleft[3]])
        
        ;>> 4. BOTTOM RIGHT:
        ;---------------------------
        ;idx = [geom.bias_full.botright[0], log[bobs[0]].naxis1-1, geom.image_trim.botright[2], geom.image_trim.botright[3]]  ;BEFORE, updated by JL 
        idx = [geom.bias_full.botright[0], log.naxis1[bobs[0]]-1, geom.image_trim.botright[2], geom.image_trim.botright[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.botright[0]:geom.bias_trim.botright[1], geom.bias_trim.botright[2]:geom.bias_trim.botright[3]])
      
      
              
        ;Save all bias images to a cube:
        ;---------------------------        
        bcube[*,*,i] = biasim
    endfor
    
    
    ;Calculating mean/median bias :
    ;---------------------------
    if master_bias eq 'mean' then begin   
        bobsMaster = mean(bcube, /double, dimen=3)          
    endif
    if master_bias eq 'median' then begin 
        bobsMaster = median(bcube, /double, dimen=3)       
    endif else stop, 'CHI_MASTERBIAS: >> ERROR << The variable master_bias can only be median or mean. Please change its value in the ctio.par file'
    
    ;Store master bias:
    ;---------------------------
    masterBiasPath = redpar.rootdir+redpar.biasdir+ redpar.date+'_'+binning+'_'+ redpar.master_bias +'_bias.fits'
    MKHDR, biasHeader, bobsMaster
    history_str1 = 'Master Bias made of ' + strtrim(string(bobsct),2) + ' frames'
    history_str2 = 'Binning of individual bias frames : ' + binning
    history_str3 = 'Speed Mode of individual bias frames : ' + rdspd
    sxaddpar, biasHeader, 'HISTORY', history_str1
    sxaddpar, biasHeader, 'HISTORY', history_str2
    sxaddpar, biasHeader, 'HISTORY', history_str3
    writefits, masterBiasPath, bobsMaster,biasHeader
    
    ;Legacy code
    ;fname = redpar.rootdir+redpar.biasdir+redpar.date+'_bin'+binsz+'_'+rdspd+'_medbias.dat'
    ;save, bobsmed, filename=fname
   
    print, 'CHI_MASTERBIAS: Master bias stored : ', masterBiasPath
    print, '  '
endif else STOP, 'CHI_MASTERBIAS: >> ERROR << Master Bias was not created. Only '+strtrim(string(bobsct),2)+' bias frames found'

END