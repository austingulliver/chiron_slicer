function bsplvn, bkpt, nord, x, ileft

;
;	Conversion of slatec utility bsplvn, used only by efc
;
;	parameter index is not passed, as efc always calls with 1
;	treat x as array for all values between ileft and ileft+1
;;

   nx = n_elements(x)

;
;	This is to break up really HUGE arrays into manageable chunks
;
   if nx GT 1000000L then begin
     lower = 0L
     upper = 639999L 
     vnikx = fltarr(nx,nord)

     while lower LT nx do begin
        print, lower, upper, nx
        vnikx(lower:upper,*) = bsplvn(bkpt, nord, x(lower:upper), $
                                      ileft(lower:upper))
        lower = upper + 1
        upper = (upper + 640000L) < (nx - 1)
     endwhile
     
     return, vnikx
   endif

   vnikx = fltarr(nx,nord)
   deltap = fltarr(nx,nord)
   deltam = fltarr(nx,nord)
   vmprev = fltarr(nx)
   vm = fltarr(nx)

   j = 0
   vnikx(*,0) = 1.

   while (j LT nord - 1) do begin

     ipj = ileft+j + 1
     deltap(*,j) = bkpt(IPJ) - x
     imj = ileft-j
     deltam(*,j) = x - bkpt(imj)
     vmprev = 0.
     for l=0,j do begin
       vm = vnikx(*,l)/(deltap(*,l) + deltam(*,j-l)) 
       vnikx(*,l) = vm*deltap(*,l) + vmprev
       vmprev = vm*deltam(*,j-l)
     endfor

     j = j + 1
     vnikx(*,j) = vmprev

   endwhile

   return, vnikx
end
