pro parkes_hi, filename, directory, magellanic=magellanic, error=error, kms=kms, save=save, cube=cube, head=head, mag2gal=mag2gal

;+
; Purpose: To convert a Parkes GASS Survey HI data cube into a structure containing
;          vel, data, var, glon, and glat tags. 
;
; filename   - Filename of the fits file in the form of a string, e.g., 'fits_gass_109_-41.fits'
; director   - Directory of fits file in the form of a string. 
; 			   If no directory is passed, then the current directory is searched for the fits file. 
; magellanic - Specify the inclusion of Magellanic coordinats in structure, with tag names mlon, mlat
; error      - To change the value used for the sensitivity of the observations. By default, this is
;              57.0e-3 K for the Parkes GASS HI survey
; kms        - Specifiy that the velocity is in km/s units instead of m/s. The default units of the GASS
;              survey is m/s, whereas the units for most others are in km/s.
; save       - Pass save file containing the hi data cube. Must also pass head.
; cube       - An array containing the HI data cube. Must also pass head.
; head       - fits file header.
; mag2gal    - Specify that the data cube coordinants are given in the Magellanic Stream Coordinant system
;              instead of the Galactic Coordinant system. 
;
; Output: A save file of name 'filename.sav' containing a structure with vel, data, var, glon, glat tags.
;         The variable name of the structure is filename, with any dash converted to an underscore.
;
; Example: parkes_hi,"fits_gass_109_-41.fits",/magellanic
;          => produces fits_gass_109_-41.sav in current directory.
;
;		   IDL> restore,"fits_gass_109_-41.sav"
;
;		   IDL> help,FITS_GASS_109__41,/str,/mag      
;		   ** Structure <a019468>, 5 tags, length=18200, data length=18200, refs=1:
;		      VEL             DOUBLE    Array[1137]
;		      DATA            FLOAT     Array[1137]
;		      VAR             FLOAT     Array[1137]
;		      GLON            FLOAT           112.183
;		      GLAT            FLOAT          -43.9700
;		      MLON            FLOAT          -103.667
;   		  MLAT            FLOAT           15.0354
;		   		   
;  Default uncertanties are 57 mK per 1 km/s channel, making the variance = (57.0e-3)^2.
;  Default is to assume the data is GASS survey.
;  See http://www.atnf.csiro.au/research/GASS/Data.html for details. 
;
; Created by Dr. Kat Barger
;-


if (NOT keyword_set(directory)) then directory = './'

if (NOT keyword_set(save)) then begin 
  filename=validate_extension(filename,'fits') 
  parkes_hi_survey=readfits(directory+filename,parkes_hi_head)
endif else if keyword_set(cube) then begin
   parkes_hi_survey=cube
endif else begin
  filename=validate_extension(filename,'sav')
  parkes_hi_survey=restore_var(filename)
endelse 

if (size(parkes_hi_survey,/type) ne 4) AND (size(parkes_hi_survey,/type) ne 5) then begin
   print,' '
   print,'*** NO FILE FOUND ***'
   print,' '
   return
endif

numi=n_elements(parkes_hi_survey[*,0,0])
numj=n_elements(parkes_hi_survey[0,*,0])
num_large=n_elements(parkes_hi_survey[0,0,*])

if keyword_set(head) then parkes_hi_head=head

delta_v=FXPAR(parkes_hi_head,'CDELT3')
ref_v=FXPAR(parkes_hi_head,'CRVAL3') ;in meters per second

if keyword_set(kms) then velocity=findgen(num_large)*delta_v+ref_v $
  else velocity=findgen(num_large)*delta_v*1e-3+ref_v*1e-3
variance=fltarr(num_large)+57.0e-3^2.0

if (NOT keyword_set(magellanic)) AND (NOT keyword_set(mag2gal)) then $
Parkes_hi=replicate({vel:velocity,data:fltarr(num_large),var:fltarr(num_large),glon:0.0,glat:0.0},numi*numj) $
else Parkes_hi=replicate({vel:velocity,data:fltarr(num_large),var:fltarr(num_large),glon:0.0,glat:0.0,mlon:0.0,mlat:0.0},numi*numj)

ref_glon=FXPAR(parkes_hi_head,'CRVAL1')
ref_pixel_glon=FXPAR(parkes_hi_head,'CRPIX1')
delta_glon=FXPAR(parkes_hi_head,'CDELT1')

ref_glat=FXPAR(parkes_hi_head,'CRVAL2')
ref_pixel_glat=FXPAR(parkes_hi_head,'CRPIX2')
delta_glat=FXPAR(parkes_hi_head,'CDELT2')


k=0.00
for i=0, long(numi)-1 do begin
    for j=0, long(numj)-1 do begin
        ;print,i,j,k
        Parkes_hi(k).data=transpose(parkes_hi_survey(i,j,*))
        nodata_index=where(Parkes_hi(k).data eq -32768.0,count_nodata)
        if count_nodata ne 0 then Parkes_hi(k).data[nodata_index]=0
        Parkes_hi(k).glon=ref_glon-delta_glon*ref_pixel_glon+delta_glon*i
        Parkes_hi(k).glat=ref_glat-delta_glat*ref_pixel_glat+delta_glat*j
        k=k+1
    endfor
endfor

if keyword_set(error) then Parkes_hi.var=(error)^2.0 $
   else Parkes_hi.var=(57.0e-3)^2.0

if keyword_set(magellanic) AND (NOT keyword_set(mag2gal)) then begin
   gal2mag,Parkes_hi.glon,Parkes_hi.glat,mlon,mlat
   Parkes_hi.mlon=mlon
   Parkes_hi.mlat=mlat
endif

if keyword_set(mag2gal) then begin
   Parkes_hi.mlon=Parkes_hi.glon
   Parkes_hi.mlat=Parkes_hi.mlat
   mag2gal,Parkes_hi.glon,Parkes_hi.mlat,glon,glat
   Parkes_hi.glon=glon
   Parkes_hi.glat=glat
endif

root=remove_extension(filename,'fits',/dash)
return_name=remove_extension(filename,'fits')

(SCOPE_VARFETCH(root,/enter,level=0))=parkes_hi

save,(SCOPE_VARFETCH(root,/enter,level=0)),filename=directory+return_name+'.sav'

end