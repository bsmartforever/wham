; MSW 6/7/05
; Script to convert multi-aperture FITS files into PAN grouped ascii format.
; Inputs are the pre-flux calibrated file, and the corresponding flux 
; calibrated version (in order to calculate the error array). If no_spax=0, 
; all spaxels are printed, if no_spax>0 then that number is printed started 
; from 51. 

PRO fits2pan, in_preflux, in_fluxcal, outfile, spax_start, spax_finish

if (n_params() ne 5) then $
	message,'Usage: fits2pan, "preflux.fits", "fluxcal.fits", "output.txt", spax_start, spax_finish (0,0=all)'
if (spax_finish lt spax_start) then message,'Spaxel range invalid'
;print,'no_spax',no_spax

; read in pre_flux calibrated file
fits_read,in_preflux,preflux,hdr1
; read in flux calibrated file
fits_read,in_fluxcal,fluxcal,hdr2

; get diminsions of each file read in
dim_size_preflux = size(preflux,/dimensions)
dim_size_preflux0 = dim_size_preflux[0]
dim_size_preflux1 = dim_size_preflux[1]
dim_size_fluxcal = size(fluxcal,/dimensions)
dim_size_fluxcal0 = dim_size_fluxcal[0]
dim_size_fluxcal1 = dim_size_fluxcal[1]
print,'      x-dimension    no of apertures'
print,dim_size_preflux[0], dim_size_fluxcal[1]

; check whether dimensions of each file match
if (dim_size_preflux[0] ne dim_size_fluxcal[0]) then begin
	print,dim_size_preflux[0], dim_size_fluxcal[0]
	message,'x dimensions of files do not match'
endif
if (dim_size_preflux[1] ne dim_size_fluxcal[1]) then begin
	print,dim_size_preflux[1], dim_size_fluxcal[1]
        message,'no of apertures do not match'
endif

; read in lambda_start and delta_lambda from FITS header 
crval1=sxpar(hdr1,'crval1')
cdelt1=sxpar(hdr1,'cdelt1')
crval2=sxpar(hdr2,'crval1')
cdelt2=sxpar(hdr2,'cdelt1')
; check whether crval1 and cdelt1 are valid header params - if not
; then return error
if (n_elements(crval1) eq 0) then $
	message,'crval1 not defined in FITS header'
if (n_elements(cdelt1) eq 0) then $
	message,'cdelt1 not defined in FITS header'
; check whether crval1=crval2 and cdelt1=cdelt2
if (crval1 ne crval2) then $
	message,'start wavelength of preflux not equal to start wavelength of fluxcal'
if (cdelt1 ne cdelt2) then $
	message,'delta lambda of preflux not equal to delta lambda of fluxcal'

print,'      crval          cdelt'
print,crval1,cdelt1

; generate wavelength grid based on header keywords
x=findgen(dim_size_fluxcal0)*cdelt1+crval1

; create the error array
; first create the % error array from the preflux data
perc_err=make_array(dim_size_preflux0,dim_size_preflux1,value=0.0)
;help,perc_err
;print,perc_err[0:13,51]
;help,preflux
;print,preflux[0:13,51]
for i=0,(dim_size_preflux1-1) do begin
 perc_err[*,i] = ( ( sqrt(abs(preflux[*,i])) ) / ( abs(preflux[*,i]) )) * 100 
endfor
;print,perc_err[0:13,51]/100
; check for divisions by 0 or sqrt(-1) and replace with 0.0
not_num=0.0
index=where(finite(perc_err) eq 0, count)
if (count gt 0) then perc_err[index]=not_num
;print,perc_err[0:15,51]/100

;multiply up all the fluxes into sensible numbers
;for j=0,(dim_size_fluxcal1-1) do begin
; fluxcal[*,j] = fluxcal[*,j]*1e18
;endfor
;print,'FLUX MULTIPLIED BY 1E18. Error percentages converted after multiplication'

; now multiply these percentages into the fluxcal data to get the error array
nerr=fltarr(dim_size_fluxcal0,dim_size_fluxcal1)
for j=0,(dim_size_fluxcal1-1) do begin
 nerr[*,j] = abs(fluxcal[*,j]) * ( perc_err[*,j] / 100 )
; nerr[*,j] = abs(fluxcal[*,j]) * 0.1
endfor

;print,nerr[0:15,51]

if ( (spax_start gt 0) or (spax_finish gt 0) ) then print,'no_spax requested =',(spax_finish-spax_start)+1
if ( (spax_start eq 0) and (spax_finish eq 0) ) then print,'all spax will be output'

; NOTE: this is how you overplot the error bars
;errplot,x,fluxcal[*,300]-sqrt(fluxcal[*,300]),fluxcal[*,300]+sqrt(fluxcal[*,300])

; now output the spectra in ascii formatted for PAN
case 1 of
; case 1: print all spax (no_spax = 0)
    ( (spax_start eq 0) and (spax_finish eq 0) ) : begin
    	 openw,1,outfile
	 printf,1,"#no of wavelength values"
	 printf,1,size(x,/dimensions)
	 printf,1,"#no of groups"
    	 printf,1,dim_size_fluxcal1
	  printf,1,"#Wavelengths"
	 printf,1,transpose([[x]])
	 printf,1,"#y values"
	 for i=0,(dim_size_fluxcal1-1) do begin
	  printf,1,(i+1)
	 endfor
	 for i=0,(dim_size_fluxcal1-1) do begin
	  printf,1,"#Spec",i
	  printf,1,transpose([[fluxcal(*,i)],[nerr(*,i)]])
	 endfor
	 close,1
	end
;case 2: print only range of spax entered
 ( (spax_start gt 0) or (spax_finish gt 0) ) : begin
   	   openw,1,outfile
	   printf,1,"#no of wavelength values"
	   printf,1,size(x,/dimensions)
	   printf,1,"#no of groups"
	   printf,1,(spax_finish-spax_start)+1
	   printf,1,"#Wavelengths"
	   printf,1,transpose([[x]])
	   printf,1,"#y values"
	   for i=spax_start,(spax_finish) do begin
	    printf,1,(i)
	   endfor
	   for i=spax_start,spax_finish do begin
            printf,1,"#Spec",i-spax_start
            printf,1,transpose([[fluxcal(*,i)],[nerr(*,i)]])
           endfor
	   close,1
	  end
 else : message, 'Error with no_spax'
endcase

END

