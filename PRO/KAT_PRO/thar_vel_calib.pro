function thar_vel_calib, line, vcen, ThAr_calib_fts,noplot=noplot
;+
; NAME:
;	ThAr_vel_calib
;
; Purpose:
;	Preforms velocity calibration using Thorium Argon WHAM Lamp observations. 
;
; Inputs:
;	line - string to specify rest wavelength. 
;	       Accepts: hb, oiii, blue_nii, hei, oi, ha, nii, sii, oii
; 	vcen - expected central LSR velocity of tune. This is to help Atlas_ThAr.pro extract
;              the correct velocity region and to help crscor to correctly match the lines. This
;              is especially important when only one bright ThAr line is present in the investigated
;              wavelength range. 
;	ThAr_calib_fits - a one element Thorium Argon calibration fts observation with the 'PROCSPEC' 
;              extension. 
;                 E.g., 
;                 dir='$HOME/WHAM/MS/data/121217/sii' 
;		  files='thcalib' 
;		  ThAr_calib=readobs(files,dir,ftsext='PROCSPEC',/ext,count=count,/quiet)
;		  ThAr_calib=ThAr_calib[0]
;       noplot - [OPTIONAL] keyword to skip plotting the resulting ThAr spectrum. 
;
; Result: 
;	Velocity calibrated array of the same length as ThAr_calib_fits.vel. This calibration accounts 
;	for the shift due to ThAr_calib_fits.vlsr.
;
; Calls:
;	Atlas_ThAr - Loads the Thorium Argon atlas at a specified wavelength
;	crscor - Locates and matches corresponding ThAr lamp observation and the ThAr atlas and 
;		 determines the wavelength shift between them. Calls CRSTRIM, CRSPROD, and CRSMAX. 
;		 This is a modified program from the GHRS IDL library. 
;		 See http://www.astro.washington.edu/docs/idl/cgi-bin/getpro/library43.html?CRSCOR
;
; Example:
;	vcen=180.
;	test=thar_vel_calib('sii',vcen,ThAr_calib,/noplot)
;	IDL> help,test
;	TEST            DOUBLE    = Array[98]
;	
; By: Dr. Kat Barger Dec 2012

   ;Get current color template and change color to B&W
   tvlct, red, green, blue, /get
   loadct,0,/silent

   ;Get current device name and change to terminal window.
   device=!d.name
   set_plot,'x'
   ;Set the ytitle orientation to 90 instead of stacking vertically stacking characters.
   font=!p.font
   !p.font=-1

theDirectory = ProgramRootDir()

restore,filename=theDirectory+'ThAr_factor.sav' ;factor_str
;normalizing from Ed's calibration lab to the WHAM insturment
factor_str.slope=factor_str.slope/factor_str.slope[0]*1.12091

CASE line OF
	'hb': BEGIN
		lambda=4861.3 & line_name='H!7b!3' & factor=factor_str.slope[10]
	END
	'oiii': BEGIN
		lambda=5006.9 & line_name='[O III]' & factor=factor_str.slope[6]
	END
	'blue_nii': BEGIN
		lambda=5754.6 & line_name='blue [N II]' & factor=factor_str.slope[8]
	END
	'hei': BEGIN
		lambda=5875.6 & line_name='He I' & factor=factor_str.slope[11]
        END
	'oi': BEGIN
	        lambda=6300.30 & line_name='[O I]' & factor=factor_str.slope[4]
	END
	'ha': BEGIN
		lambda=6562.8 & line_name='H!7a!3' & factor=factor_str.slope[0]
	END
	'nii': BEGIN
		lambda=6583.4 & line_name='[N II]' & factor=factor_str.slope[8]
	END
	'sii': BEGIN
		lambda=6716.4 & line_name='[S II]' & factor=factor_str.slope[9]
	END
	'oii': BEGIN
	        lambda=7319.10 & line_name='[O II]' & factor=factor_str.slope[5]
        END
ENDCASE

;Factor doesn't seem necessary. The RAW to PROCSPEC reduction pipeline seems to take care of any required 
;wavelength scale factor. 
factor=1.0

;Determine the velocity shifted wavelength.
wavelength=wavelength(vcen,wavelength=lambda,/quiet)
;Specify the wavelength range to extract from the ThAr atlas.
wavelength_range=[wavelength(vcen-150.,wavelength=lambda,/quiet),wavelength(vcen+150.,wavelength=lambda,/quiet)]
;Define wavelength width of that region, as required for the Atlas_ThAr function.
wavelength_width=abs(wavelength_range[1]-wavelength_range[0])

;Convert from velocity to wavelength. 
c=299792. ;km/s
;Shift ThAr_calib_fts_wave.vel to the ideal tune velocity and convert to wavelengths.
;This shift will be used to provide a better comparison between the ThAr lamp and atlas lines. 
ThAr_calib_fts_wave=lambda*(c/(ThAr_calib_fts.vel+vcen/2.0-ThAr_calib_fts.vel[0]))/((c/(ThAr_calib_fts.vel+vcen/2.0-ThAr_calib_fts.vel[0]))-1)

;Load the Thorium Argon atlas at the shifted wavelength, i.e., rest wavelength shifted by vcen.
;The resultant ThAr_spectra structure has wavelength and intensity elements of an arbitrary length. 
ThAr_spectra=Atlas_ThAr(wavelength,wavelength_width,/noplot)

    ;Rebin to the atlas to same wavelength spacing and coverage as the lamp observation.  
    xnew=ThAr_spectra.wave
    xold=ThAr_calib_fts_wave*factor-(ThAr_calib_fts_wave[0])*(factor-1.0)
    yold=(ThAr_calib_fts.data-min(ThAr_calib_fts.data))/max(ThAr_calib_fts.data-min(ThAr_calib_fts.data))
    ynew=interpol(yold,xold,xnew)

    ;Find and remove elements that extend past original wavelength coverage of the atlas array.
    ;The net result of this and the previous step is an atlas array with the same wavelength 
    ;spacing as the lamp observations, but not necessarily the same number of elements. 
    low_loc=where(xnew lt xold[0],num_low)
    if num_low ne 0 then remove,low_loc,xnew,ynew
    high_loc=where(xnew gt xold[n_elements(xold)-1],num_high)
    if num_high ne 0 then remove,high_loc,xnew,ynew

    ;Convert from double to float to avoid errors when calling the crscor function. 
    xnew=float(xnew) & ynew=float(ynew)

    ;Overlap wavelengths of the lamp and atlas arrays. 
    min_wave=float(max([ThAr_spectra.wave[0],xnew[0]]))
    max_wave=float(min([ThAr_spectra.wave[n_elements(ThAr_spectra.wave)-1],xnew[n_elements(xnew)-1]]))

    ;Broaden the atlas spectra so that the resolution is closer to the lamp observations by convolving the spectrum with a Gaussian of width
    ;2 Angstroms. This changes the width to sqrt((original_with)^2+(convolved_width)^2). This is done to promote the proper matching.  
    ;of the lamp and atlas lines. 
    sigma=2.
    x=indgen(n_elements(10))
    x_template=float(ThAr_spectra.wave)
    y_template=float((ThAr_spectra.data-min(ThAr_spectra.data))/max(ThAr_spectra.data-min(ThAr_spectra.data)))
    y_template=(convol(y_template,exp(-.5*((x-n_elements(x)/2.0)/sigma)^2),1)-min(convol(y_template,exp(-.5*((x-n_elements(x)/2.0)/sigma)^2),1)))/max(convol(y_template,exp(-.5*((x-n_elements(x)/2.0)/sigma)^2),1)-min(convol(y_template,exp(-.5*((x-n_elements(x)/2.0)/sigma)^2),1)))

    ;Determine the wavelength offset between the atlas and lamp arrays.
    offset=crscor(x_template,y_template,xnew,ynew,min_wave,max_wave,10,delv)

    if NOT Keyword_set(noplot) then begin
       plot,x_template,y_template,xtitle='Wavelength [Angstroms]',ytitle='Normalized Intensity [Intensity Units/(km s!U-1!N)]',xstyle=9
       oplot,xnew+offset,ynew,color=fsc_color('green') 

       ;oplot,ThAr_calib_fts_wave*factor-(ThAr_calib_fts_wave[0])*(factor-1.0)+offset*factor+(xnew[0]-ThAr_calib_fts_wave[0]),(ThAr_calib_fts.data-min(ThAr_calib_fts.data))/max(ThAr_calib_fts.data-min(ThAr_calib_fts.data)),color=fsc_color('orchid')

       oplot,[1,1]*lambda,!y.crange,linestyle=1
       oplot,[1,1]*wavelength,!y.crange,linestyle=2
       axis,!x.crange[0],!y.crange[1],xax=1,xrange=(1.-lambda/!x.crange)*c,xstyle=1,xtitle='velocity [km s!U-1!N]'
       legend,['Atlas','Lamp','vcen'],linestyle=[0,0,2],color=[fsc_color('white'),fsc_color('green'),fsc_color('white')],box=0,/right

    endif

   ;Change color template back to original color template
   tvlct, red, green, blue

   ;Set device back to original device.
   set_plot,device
   ;Set device font back to original font. 
   !p.font=font

;return the velocity calibrated ThAr_calib_fts.vel array, accounting for the vlsr shift. Much of the funky math below is to account for the shift to 
;the ideal tune velocity to match the lamp and atlas lines. 
return,(1.-lambda/(ThAr_calib_fts_wave*factor-(ThAr_calib_fts_wave[0])*(factor-1.0)+offset*factor+(xnew[0]-ThAr_calib_fts_wave[0])))*c-ThAr_calib_fts.vlsr

end
