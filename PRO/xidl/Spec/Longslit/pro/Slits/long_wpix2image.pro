;+
; NAME:
;   long_wpix2image
;
; PURPOSE:
;  Convert a wavelength solution to a wavelength image ??
;
; CALLING SEQUENCE:
; wvimg =  long_wpix2image( tset2d, tset_slits, wset = , xshift = $
;    waveimg = , XFIT = )
;
; INPUTS:
;  tset2d=  -- Trace set describing the arc line curvature
;
; OUTPUTS:
; 
; OPTIIONAL OUTPUTS:
;  waveimg=
;
; COMMENTS:
;
; EXAMPLES:
;
; BUGS:
;
; PROCEDURES CALLED:
;
; REVISION HISTORY:
;   20-Apr-2005  Written by J. Hennawi Berkeley
;-
;------------------------------------------------------------------------------
function long_wpix2image, tset2d, tset_slits $
  ,wset = wset, xshift = xshift $
  , waveimg = waveimg, XFIT = XFIT

   if N_PARAMS() LT 2 then begin
     print, 'Syntax: wpix = wpix2image(tset2d, tset_slits)'
     return, 0
   endif

   if n_elements(tset_slits) NE 2 then begin
      splog, 'tset_slits must be a 2 element structure of the slit edges'
      return, 0
   endif

   traceset2xy, tset_slits[0], rows, left_edge
   traceset2xy, tset_slits[1], rows, right_edge
   if (keyword_set(xshift)) then begin
      left_edge = left_edge + xshift
      right_edge = right_edge + xshift
   endif

   maskim  = long_slits2mask(tset_slits, xshift=xshift)
   slitpos = long_slits2x(tset_slits, xshift=xshift)

   edge_sep = right_edge - left_edge

   med_width = djs_median(edge_sep,1)
   slit_order = reverse(sort(med_width))
   nslit = n_elements(slit_order)

   if nslit NE n_elements(tset2d) then begin
     splog, 'WARNING: Nslits in tset_slits does not equal the number in tset2d'
   endif

   dims = tset2d[0].dims
   pix_image = dblarr(dims[0], dims[1])

   if keyword_set(wset) OR keyword_set(xfit) then $
     waveimg = dblarr(dims[0], dims[1])
   
   for islit = 0, nslit-1 do begin

;     islit = slit_order[ii]

     coeff = tset2d[islit].coeff2d

     ; only take non-zero coefficients
     nxcoeff = max(where(total(abs(coeff),1) GT 0)) + 1  
     nycoeff = max(where(total(abs(coeff),2) GT 0)) + 1

     if nxcoeff EQ 0 OR nycoeff EQ 0 then begin
       splog, 'WARNING: coefficient numbers make no sense', $
              islit+1L, nxcoeff, nycoeff 
       continue
     endif

     in = where(maskim EQ islit + 1,nin)
     if nin EQ 0 then begin
       splog, 'WARNING: This slit has no pixels mapped', islit
       continue
     endif

     slit_frac = slitpos[in]
     t = 2.0D*(double(in/dims[0]) - double(tset2d[islit].xmin))/ $
       double(tset2d[islit].xmax - tset2d[islit].xmin) - 1.0D

     y = 2.0D*(double(slit_frac) - double(tset2d[islit].ymin))/ $
       double(tset2d[islit].ymax - tset2d[islit].ymin) - 1.0D
     
     if tset2d[islit].func EQ 'legendre' then begin
         tbasis = flegendre(t, nxcoeff)
         ybasis = flegendre(y, nycoeff)
     endif else begin
       splog, 'Not sure which basis function is being used'
       continue
     endelse

     for ix=0, nxcoeff-1 do $
      for iy=0, nycoeff-1 do $
        pix_image[in] = pix_image[in] + $
            tbasis[*,ix] * ybasis[*,iy] * coeff[ix,iy]        
     if keyword_set(wset) then begin
         if tag_exist(wset[islit],'ORDER') then order = wset[islit].ORDER $
         else order = n_elements(wset[islit].COEFF)  ;; JXP kludge
         tset = $
           { FUNC:  wset[islit].FUNC, $
             XMIN:  wset[islit].XMIN, $
             XMAX:  wset[islit].XMAX, $
             COEFF: wset[islit].COEFF[0:order-1L> 0] $
           }
         traceset2xy, tset, pix_image[in], temp_wave
         waveimg[in] = temp_wave
     endif
     IF keyword_set(XFIT) THEN $
       waveimg[in] = x_calcfit(pix_image[in], fitstr = xfit[islit])
     
 endfor


   return, pix_image
end
