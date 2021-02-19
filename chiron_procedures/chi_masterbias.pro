;+
;
;  NAME: 
;     chi_medianbias
;
;  PURPOSE: 
;    To create and store median bias frames for the various modes for 
;	 bias subtraction
;
;  CATEGORY:
;      CHIRON
;
;  CALLING SEQUENCE:
;
;      chi_medianbias
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
;      chi_medianbias, redpar = redpar, log = log, /bin31, /normal
;
;  MODIFICATION HISTORY:
;        c. Matt Giguere 2012.04.17 04:09:19 PM
;           Jorge Lozano 2020.10.13 04:09:19 PM :  master bias using mean option created  
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
master_bias = master_bias  ; Takes 2 options "  'median' or 'mean'

if keyword_set(bin31) then binsz='31'
if keyword_set(bin11) then binsz='11'
if keyword_set(bin44) then binsz='44'
if keyword_set(normal) then rdspd = 'normal'
if keyword_set(fast) then rdspd = 'fast'

;------------------------------------------
;>> Filter files according to variables set 
;------------------------------------------
bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '3 1' and strt(log.speedmod) eq 'normal', bobsct)

if keyword_set(bin11) and keyword_set(normal) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '1 1' and strt(log.speedmod) eq 'normal', bobsct)
endif

if keyword_set(bin11) and keyword_set(fast) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '1 1' and strt(log.speedmod) eq 'fast', bobsct)
endif

if keyword_set(bin31) and keyword_set(fast) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '3 1' and strt(log.speedmod) eq 'fast', bobsct)
endif

if keyword_set(bin44) and keyword_set(normal) then begin
  bobs = where(strt(log.object) eq 'bias' and strt(log.ccdsum) eq '4 4' and strt(log.speedmod) eq 'normal', bobsct)
endif




;------------------------------------------
;>> Construct Master Bias 
;------------------------------------------
if bobsct gt 2 then begin
    print, 'CHI_MEDIANBIAS: Creating Master Bias made of '+strtrim(string(bobsct),2)+' frames. Binning used: '+binsz+' .SpeedMode used : '+rdspd     
    
      
    ;bcube = dblarr(long(log[bobs[0]].naxis1), long(log[bobs[0]].naxis2), bobsct)    
    bcube = dblarr(long(log.naxis1[bobs[0]]), long(log.naxis2[bobs[0]]), bobsct)  ; modfied by JL to make it compatibla with creation of "createlogstruc"
    
    for i=0, bobsct-1 do begin
        ;biasim = double(readfits(log[bobs[i]].filename, hd)) ; by JL
        biasim = double(readfits(log.filename[bobs[i]], hd))
        geom = chip_geometry(hdr=hd)
        
        ;1. subtract median value from upper left quadrant (both image and overscan region):
        ;  --------------------------------------------------------------------------------
        idx = [0L, geom.bias_full.upleft[1], geom.image_trim.upleft[2], geom.image_trim.upleft[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.upleft[0]:geom.bias_trim.upleft[1], geom.bias_trim.upleft[2]:geom.bias_trim.upleft[3]])
        
        ;>>2. now do the same for the upper right quadrant:
        ;  -----------------------------------------------
        ;idx = [geom.bias_full.upright[0], log[bobs[0]].naxis1-1, geom.image_trim.upright[2], geom.image_trim.upright[3]] ;BEFORE, updated by JL
        idx = [geom.bias_full.upright[0], log.naxis1[bobs[0]]-1, geom.image_trim.upright[2], geom.image_trim.upright[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.upright[0]:geom.bias_trim.upright[1], geom.bias_trim.upright[2]:geom.bias_trim.upright[3]])
        
        ;>> 3. and the bottom left quadrant:
        ;----------------------------------
        idx = [0L, geom.bias_full.botleft[1], geom.image_trim.botleft[2], geom.image_trim.botleft[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.botleft[0]:geom.bias_trim.botleft[1], geom.bias_trim.botleft[2]:geom.bias_trim.botleft[3]])
        
        ;>> 4. now the bottom right:
        ;---------------------------
        ;idx = [geom.bias_full.botright[0], log[bobs[0]].naxis1-1, geom.image_trim.botright[2], geom.image_trim.botright[3]]  ;BEFORE, updated by JL 
        idx = [geom.bias_full.botright[0], log.naxis1[bobs[0]]-1, geom.image_trim.botright[2], geom.image_trim.botright[3]]
        biasim[idx[0]:idx[1], idx[2]:idx[3]] -= $
        	median(biasim[geom.bias_trim.botright[0]:geom.bias_trim.botright[1], geom.bias_trim.botright[2]:geom.bias_trim.botright[3]])
      
      
      
      
      
      
        ;Save all bias images to a cube:
        ;---------------------------        
        bcube[*,*,i] = biasim
    endfor
    
    if master_bias eq 'mean' then begin   
        bobsmed = mean(bcube, /double, dimen=3)  
        frame_name= 'MEAN' 
    endif
    if master_bias ew 'median' then begin 
        bobsmed = median(bcube, /double, dimen=3)
        frame_name= 'MEDIAN'
    endif else stop, 'CHI_MASTERBIAS: The variable master_bias can only be median or mean. Please change its value in the ctio.par file'
    
    
    ;PRINT, 'Size of Master Bias created: '
    ;rint, size(bobsmed)
    fname = redpar.rootdir+redpar.biasdir+redpar.date+'_bin'+binsz+'_'+rdspd+'_medbias.dat'
    save, bobsmed, filename=fname
    
    ;To Export file as a fits file : used to share info
    ;writefits, 'C:\Users\mrstu\Desktop\School\research_Physics\yale_software\chiron\files_sent_dr_gulliver\181103_master_bias.fits', bobsmed
    
    print, 'Master '+frame_name +' bias stored in ', strtrim(fname,2)
    print, '  '
endif else print, 'CHI_MASTERBIAS: Master Bias was made of 1 file'

END