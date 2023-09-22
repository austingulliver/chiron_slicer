pro jplaneteph,targ,cent,jdt,pos,vel,ephemeris=ephemeris,mks=mks,$
               noclean=noclean,barydir=barydir

; Computes the position and velocity of TARGET with respect to CENTER
; (in AU and AU/DAY) using JPL Planetary Ephemeris.  Drives programs
; JPLEPHREAD & JPLEPHINTERP    ; replaces planeteph.pro   with all IDL version

;IDL>   JPLEPHREAD, 'JPLEPH.200', pinfo, pdata, [2451544D, 2451545D]
;IDL>   JPLEPHINTERP, pinfo, pdata, 2451544.5D, xearth, yearth, zearth, $
;                 /EARTH, posunits='AU'
; Sample jdrange: [2451544D, 2451545D]

target = strupcase(targ) & center = strupcase(cent)
if target  eq 'SSB' then target = 'SOLARBARY'
if center  eq 'SSB' then center = 'SOLARBARY'


spawn, 'hostname', host
spawn, 'cd', pwddir   ;Updated to a Windows command

if  host eq 'Drius22' then begin
  ephemeris = 'C:\Users\mrstu\idlworkspace_yalecalibration\chiron_procedures\JPLEPH.405'
endif else if pwddir eq 'C:\Users\gulliver' then begin
  ephemeris = 'C:\F disk\chiron_reduc_pipeline\chiron_procedures\JPLEPH.405'
endif else if host eq 'DESKTOP-41NKOUC' then begin
  ephemeris = 'C:\Users\aleja\Desktop\Desktop\Job\Gulliver\Reduction-Pipeline-Software\chiron_reduc_pipeline\chiron_procedures\JPLEPH.405'
endif else   stop, ' Set up the environment variable  path ephemeris in JNUTATION into the code to continue.'


if (file_test(ephemeris))(0) eq '' then stop,ephemeris+' not found. Wrong directory?'   ; Outdated; findfile(ephemeris)
jdrange = [jdt-0.5d0,jdt+0.5d0]   ; Must feed ephem range over which to interp.


jplephread, ephemeris, pinfo, pdata, jdrange

jplephinterp, pinfo, pdata, jdt, xpos, ypos, zpos, Vx, Vy, Vz, objectname = $
   target, center = center,posunits='AU',/velocity,velunits = 'AU/DAY'

pos = [xpos,ypos,zpos]
vel = [Vx, Vy, Vz]

end

;IDL>   JPLEPHINTERP, pinfo, pdata, 2451544.5D, xearth, yearth, zearth, $
;                 /EARTH, posunits='AU'
