pro inormsave_input, file

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: To create an idl save file of a spectrum enclosed in a 
;		   fits file to the format expected by inormsave.pro. 
;		
;		   Use inormsave.pro to convert the inormsave_input.pro
;		   input to iNorm.pro input. 
;
; Input:  
;		  file - A fits file containing a large spectrum to be
;		         partitioned into smaller spectrum files for 
;				 many comman lines in the format anticipated by
;				 iNorm, e.g., OVI1031.9i.save
;
; Output: Save file named object.sav, where object is defined in 
;		  the header file of the supplied fits file. This save
;		  file will include the redshift,object,vlsr,wave,flux,err.
;		  
;		  When calling inormsav with this output file, the redshift,
;		  object, and vlsr keywords don't have to be specified.
;		  E.g., inormsave,root='object.sav'
;
;
; Created by Dr. Kat Barger 09/2013
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

file=validate_extension(file,'fits')

;Creates spectrum structure with wave, flux, and error tags
spectrum = mrdfits(file,1,header,/fscale)
;Gets more header info to extract position and object name.
a=mrdfits(file,0,hdr0,/silent)

ra = float(strtrim(sxpar(hdr0,'RA_TARG'),2))*1.d  ;deg
dec = float(strtrim(sxpar(hdr0,'DEC_TARG'),2))*1.d  ;deg
object = strtrim(sxpar(hdr0,'TARGNAME'),2)
redshift = strtrim(sxpar(hdr0,'Z'),2)

glactc,ra,dec,2000,gl,gb,1,/degree
lsrvel, gl, gb, vlsr, vmb,/silent

wave=spectrum.wave
flux=spectrum.flux
sigma_up=spectrum.error
err=spectrum.error

print,'Saving file: '+object+'.save'
save,redshift,object,vlsr,wave,flux,err,file=object+'.save'

end