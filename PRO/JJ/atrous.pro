function atrous, spec, filter = filter, n_scales = n_scales, check = check
;+
; NAME:
;   ATROUS
; PURPOSE:
;   Perform a 1-D "a trous" wavelet decomposition on an image.
;
; CALLING SEQUENCE:
;   decomposition = ATROUS( spectrum [, filter = filter,  $
;                           n_scales = n_scales, /check]
;
; INPUTS:
;   SPECTRUM -- A 1-D Image to Filter
;
; KEYWORD PARAMETERS:
;   FILTER -- A 1D (!!) array representing the filter to be used.
;             Defaults to a B_3 spline filter (see Stark & Murtaugh
;             "Astronomical Image and Data Analysis", Spinger-Verlag,
;             2002, Appendix A)
;   N_SCALES -- Set to name of variable to receive the number of
;               scales performed by the decomposition.
;   CHECK -- Check number of scales to be performed and return.
; OUTPUTS: 
;   DECOMPOSITION -- A 2-D array with scale running along the 2nd axis
;                    (large scales -> small scales). The first plane
;                    of the array is the smoothed image. To recover
;                    the image at any scale, just total the array
;                    along the 2nd dimension up to the scale desired.
;                  
;
; RESTRICTIONS:
;   Uses FFT convolutions which edge-wrap instead of mirroring the
;   edges as suggested by Stark & Mutaugh.  Wait for it.
;
; MODIFICATION HISTORY:
;
;       Mon Oct 6 11:49:50 2003, Erik Rosolowsky <eros@cosmic>
;		Written.
;  24 April 2005, JohnJohn: Rewrote as a function, converted to work
;  with 1D spectra rather than 2D images.
;
;-

on_error, 2
if n_elements(filter) eq 0 then filter = 1./[16, 4, 8/3., 4, 16]
nel = n_elements(spec)

if 1-keyword_set(n_scales) then $
  n_scales = floor((alog(nel)/n_elements(filter))/alog(2))
decomp = fltarr(nel, n_scales+1)

sp = spec
for k = 0, n_scales-1 do begin
; Smooth image with a convolution by a filter    
    smooth = convol(sp, filter, /edge_wrap)
;    num_conv, sp, filter, smooth
    decomp[0, n_scales-k] = sp-smooth
    sp = smooth

; Generate new filter
    newfilter = fltarr(2*n_elements(filter)-1) 
    newfilter[2*findgen(n_elements(filter))] = filter
; note filter is padded with zeros between the images
    filter = newfilter
endfor

                                ; Stick last smoothed image into end of array
decomp[*, 0] = smooth

return, decomp
end
