Function atlas_thar,wavelength,width,noplot=noplot,vlsr=vlsr
;+
; NAME:
;	Atlas_ThAr
;
; Purpose:
;	To load the Thorium Argon atlas surrounding a desired wavelength, padded by
;	a specified wavelength span. 
;
; Inputs:
;	wavelength - numerical wavelength in angstroms.
; 	width - numerical wavelength width in angstroms.
;       noplot - [OPTIONAL] keyword to skip plotting the resulting ThAr spectrum. 
;       vlrs - [OPTIONAL] keyword used to shift the velocity to vlsr frame. 
;
; Result: 
;	A one element structure containing the the wavelength and the intensity of the ThAr atlas over the 
;	specified wavelength region. Unless /noplot is specified, the spectrum will be plotted in the 
;	current window with a vertical dotted line representing the specified central wavelength.
;
; Example:
;	temp=Atlas_ThAr(6562.8,50)  
;	IDL> help,temp,/str
;	** Structure <1591998>, 2 tags, length=5392, data length=5388, refs=1:
;	   WAVE            DOUBLE    Array[449]
;	   DATA            FLOAT     Array[449]
;
; By: Dr. Kat Barger Dec 2012

;Function called by ThAr_vel_calib

theDirectory = ProgramRootDir()

ThAr=readfits(theDirectory+'ThAr.fits',header,/silent)

;Thorium Argon Atlas begins at 3200 Angstroms and has an element spacing of 0.11129498134553D Angstroms.
start=3200. 
delta=0.11129498134553D 

wavelength_arr=findgen(66473)*delta+start

region=where((wavelength_arr ge wavelength-width/2.0) and (wavelength_arr le wavelength+width/2.0))


if NOT Keyword_set(noplot) then begin
   !p.region=[0.1,0.1,0.9,0.9]
   plot,wavelength_arr[region],(ThAr[region]-min(ThAr[region]))/(max(ThAr[region]-min(ThAr[region]))),$
        xrange=[wavelength-width/2.0,wavelength+width/2.0],xstyle=8,$
        xtitle='Wavelength [Angstrom]',ytitle='Normalized intensity',charsize=1.2
   oplot,[1,1]*wavelength,!y.crange,linestyle=1

   min=wavelength(!x.crange[0],wavelength=wavelength,/inverse)
   max=wavelength(!x.crange[1],wavelength=wavelength,/inverse)

   if keyword_set(vlsr) then begin
      min=min-vlsr
      max=max-vlsr
   endif
   axis,0,!y.crange[1],/xaxis,xrange=[min,max],xstyle=1,xtitle='velocity [km s!U-1!N]',charsize=1.2

   xyouts,0.55,0.93,'Thorium Argon Atlas',/normal,align=0.5,charsize=2

   ;pause

endif

ThAr_str=replicate({wave:wavelength_arr[region],data:ThAr[region]},1)

return,ThAr_str;

end