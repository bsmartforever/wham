; written by MSW
; 24/4/05

function opan_readFitsGroup,filename,redshift
if n_params() eq 0 then return,(-1)

;print,'Redshift=',redshift

; read in the fits file selected and extract some dimension info
fits_read,filename,z,hdr
dim_size = size(z,/dimensions)
if size(dim_size,/dimensions) eq 1 then begin
  dim_size1=1
endif else begin
    dim_size1 = dim_size[1]
endelse
dim_size0 = dim_size[0]
print,'      x-dimension  no of apertures'
print,dim_size0, dim_size1

; read in lambda_start and delta_lambda from FITS header 
crpix1=sxpar(hdr,'crpix1')
crval1=sxpar(hdr,'crval1')
cdelt1=sxpar(hdr,'cdelt1')
; check whether crval1 and cdelt1 are valid header params
if (n_elements(crpix1) eq 0) then $
        message,'crpix1 not defined in FITS header'
if (crpix1 ne 1.) then $
        print,'WARNING: CRPIX1 not equal to 1.0'
if (n_elements(crval1) eq 0) then $
        message,'crval1 not defined in FITS header'
if (n_elements(cdelt1) eq 0) then $
        message,'cdelt1 not defined in FITS header'
print,'      crval        cdelt'
print,crval1,cdelt1

; generate wavelength grid based on header keywords
if crpix1 eq 1.0 then $ ;simple case
  x=findgen(dim_size0)*cdelt1+crval1
if crpix1 ne 1.0 then begin
  crval_start=crval1-(crpix1*cdelt1) ;wavelength of x start
  x=findgen(dim_size0)*cdelt1+crval_start
endif
v=float(redshift)
;x*=(1-v/3e5)^0.5/(1+v/3e5)^0.5  ;relativistic redshift correction
x=x/(1+(v/3e5))   ;non-relativistic redshift correction (* 1/(1+z) )


;-----------------------------------------------------------------------------
; Make up error array
; Not the most ideal situation, but it has to be
;-----------------------------------------------------------------------------
zerr=make_array(dim_size0,dim_size1,value=0.0)
; if there is only 1 spectrum: create a fixed error array with a value of 0.2% of flux that is <= 1000x the median
if dim_size1 eq 1 then begin
  z_med = median(z[*]) ;get spectrum median
  if z_med eq 0.0 then z_med=mean(z[*])  ;guard against very strange exceptions where median is 0.0
  z_sort=sort(z[*]) ;sort flux values into ascending order (i.e. last index=peak value)
  ;loop through the sorted indexes until the value reaches some factor above the median (1000x)
  ;and output this index
  index=0
  for j=0,n_elements(z_sort)-1 do begin
    if z[z_sort[j]] lt 1000*abs(z_med) then begin
      index=j
    endif
  endfor
  ;use this recorded index to extract out the "real" peak and convert this to an error value
  zerr[*] = z[z_sort[index]]*0.005 ;error = 0.2% of real peak value
endif else begin
;when there is >1 spectrum: create a fixed error array for each spectrum with a value of 0.2% of flux that is <= 1000x the median
  for i=0,(dim_size1-1) do begin
    z_med = median(z[*,i]) ;get spectrum median
    if z_med eq 0.0 then z_med=mean(z[*,i])  ;guard against very strange exceptions where median is 0.0
    z_sort=sort(z[*,i]) ;for each spectrum, sorts into ascending order (i.e. last index=peak value)
    ;=> peak at x[z_sort[0]],z[z_sort[0],i]
    ;loop through the sorted indexes until the value reaches some factor above the median (1000x)
    ;and output this index
    index=0
    for j=0,n_elements(z_sort)-1 do begin
      if z[z_sort[j],i] lt 1000*abs(z_med) then begin
        index=j
      endif
    endfor
    ;use this recorded index to extract out the "real" peak and convert this to an error value
    zerr[*,i] = z[z_sort[index],i]*0.005 ;error = 0.2% of real peak value
  endfor
endelse


;go through and remove infinities
not_num=1
index=where(finite(zerr) eq 0, count)
if (count gt 0) then zerr[index]=not_num
print,''
print,'Mean of error array ',mean(zerr)
if count gt 0 then print,'Number of NaNs ',count

; Create the spax no array
y = indgen(dim_size1)

; put all the variables into a structure (conforms to format of other procedures)
d = {x:x,y:y,z:z,zerr:zerr}

return,d
end
