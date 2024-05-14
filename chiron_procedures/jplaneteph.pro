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

ephemeris = getenv('CHIRON_JPLEPH_PATH')
if ephemeris eq '' then message, 'Before running the pipeline you need to set the environment variable CHIRON_JPLEPH_PATH to be equal to the full path for your jpleph.405 file.'

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
