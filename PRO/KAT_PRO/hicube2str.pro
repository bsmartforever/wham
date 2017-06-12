pro hicube2str, filename, directory, magellanic=magellanic, error=error,$
                save=save, cube=cube, head=head, mag2gal=mag2gal,$
                ms2kms=ms2kms, k2cm2=k2cm2,kms=kms

;+
; Purpose: To convert an HI data cube or image into a structure containing
;          vel, data, var, glon, and glat tags. Optional tags include mlon and mlat.
;
; filename   - Filename of the fits file in the form of a string, e.g., 'fits_gass_109_-41.fits'
; director   - Directory of fits file in the form of a string. 
; 			       If no directory is passed, then the current directory is searched for the fits file. 
; magellanic - Specify the inclusion of Magellanic coordinats in structure, with tag names mlon, mlat
; error      - Specifies the error per velocity bin to use for calculating the variance. 
;              The program searches for the 'RMS' keyword or for a keyword that specifies the 
;              GASS Galactic HI Survey by default. If GASS is specified in the header, the program will assume
;              that the uncertanties are 57 mK per 1 km/s channel, making the variance = (57.0e-3)^2.
;              Otherwise the program will not produce a variance tag in the output structure. 
; kms        - Specifiy that the velocity is in km/s units instead of m/s. If 'km/s' is specified in 
;              the keyword CUNIT3, then no conversion will be made. The default assumption is that the supplied
;              units are in 'm/s as that's the units of the GASS datacubes. 
;              survey is m/s, whereas the units for most others are in km/s.
; save       - Pass save file containing the hi data cube. Must also pass head.
; cube       - An array containing the HI data cube. Must also pass head.
; head       - fits file header.
; mag2gal    - Specify that the data cube coordinants are given in the Magellanic Stream Coordinant system
;              instead of the Galactic Coordinant system. 
; ms2kms     - Convert from m/s to km/s. This is done automatically when the units are given, but
;              often times they are not specified. The program will inform the user if the velocity 
;              seems unusually large or small to ask if the velocity is actually in m/s. 
; k2cm2      - Convert from Kelvin to cm^2. Default is Kelvins.
;
; Output: A save file of name 'filename.sav' containing a structure with the following tags:
;             vel   - velocity. If heliocentric is supplied in the header, the program will convert the velocity 
;                     to the kinematic local standard-of-rest velocity in km/s
;             data  - emission strength of the HI gas in Kelvin
;             var   - variance, a.k.a. the square of the uncertainty per velocity bin 
;             glon  - Galactic Longitude in degrees
;             glat  - Galactic Latitude in degrees
;             mlon  - Magellanic Stream Longitude in degrees
;             mlat  - Magellanic Strema Llatitude in degrees
;
;         The variable name of the structure is filename, with any dash converted to an underscore.
;
; Example: hicube2str,"fits_gass_109_-41.fits",/magellanic
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
; Dependencies:
;   mag2gal   - Converts Galactic coordinants to Magellanic Stream Coordinants (Author David Nidever)
;   choice    - Asks users to specify yes or no to clarify unusual values (Author Kat Barger)
;   rdhd      - Converts from JY/Beam to Kelvin units
;   xyad      - Converts from ra and dec to Galactic coordinants. Handles unusual, non-linear mapping. 
;   helio2lsr - Converts from heliocentric to kinematic local standard-of-rest frame.
; 
; Created by Dr. Kat Barger 2013
; 
; Modifications:
;  - Added more flexibility for using datacubes from multiple sources.
;    - Added the ms2kms option and a search for the units if the exist in the header
;    - Added a search for Heliocentric in the header keyword CTYPE3
;    - Added better ra and dec conversion to glon and glat by also including non-linear
;      projections, e.g., ra--sin and dec--sin
;    - Searches for units specified in the BUNIT header keyword. If JY/BEAM are given,
;      convert to Kelvin. 
;    - Added k2cm2 keyword to set units in cm^2 instead of Kelvin.
;
;-


if (NOT keyword_set(directory)) then directory = './'

if (NOT keyword_set(save)) then begin 
  filename=validate_extension(filename,'fits') 
  hi_cube_survey=readfits(directory+filename,hi_cube_head)
endif else if keyword_set(cube) then begin
   hi_cube_survey=cube
endif else begin
  filename=validate_extension(filename,'sav')
  hi_cube_survey=restore_var(filename)
endelse 

if (size(hi_cube_survey,/type) ne 4) AND (size(hi_cube_survey,/type) ne 5) then begin
   print,' '
   print,'*** NO FILE FOUND ***'
   print,' '
   return
endif

if (NOT keyword_set(error)) then error = 0.0

numi=n_elements(hi_cube_survey[*,0,0])
numj=n_elements(hi_cube_survey[0,*,0])
num_large=n_elements(hi_cube_survey[0,0,*])

if keyword_set(head) then hi_cube_head=head

;Check to make sure the image is in units of Kelvin
image_units=FXPAR(hi_cube_head,'BUNIT')

if (NOT strmatch(image_units,'K*',/fold_case)) AND (keyword_set(image_units)) then begin
  ;If in JY/BEAM, convert to Kelvin
  if (strmatch(image_units,'*JY/BEAM*',/fold_case)) then begin
    rdhd,hi_cube_head,structure=convert2k
    hi_cube_survey=hi_cube_survey*convert2k.JYPB2K
  endif else begin
    print,' '
    print,'*** Invalid image units ***'
    print,'Units: ',image_units
    print,' '
    return
  endelse
endif

delta_v=FXPAR(hi_cube_head,'CDELT3')
ref_v_pixel=FXPAR(hi_cube_head,'CRPIX3')
ref_v=FXPAR(hi_cube_head,'CRVAL3') ;in meters per second if GASS Survey
vel_units=FXPAR(hi_cube_head,'CUNIT3') ;in meters per second if GASS Survey
if (size(vel_units,/type) ne 2) then ms_flag=strmatch(vel_units,'M/S*',/fold_case)
if keyword_set(ms2kms) then ms2kms=1e-3 else ms2kms=1.0
if ((size(ms_flag,/type)) ne 0) then $
   if (ms_flag) then ms2kms=1e-3 

object=FXPAR(hi_cube_head,'OBJECT')
if (strmatch(object,'*GASS Galactic HI Survey*',/fold_case)) AND (NOT keyword_set(error)) then error=57.0e-3

RMS=FXPAR(hi_cube_head,'RMS')
if ((size(ms_flag,/type)) ne 0) AND (NOT keyword_set(error)) then error=RMS

if num_large ne 1 then begin
;Datacube, include velocity

  velocity=findgen(num_large)*delta_v*ms2kms+ref_v*ms2kms 
  if size(ref_v_pixel,/type) eq 5 then velocity=velocity-ref_v_pixel*delta_v*ms2kms

  if max(abs([min(velocity),max(velocity)]) /1000.) ge 1. then begin
    print,' '
    print,'*** The min and max velocity are very large: ', min(velocity),'and', max(velocity),format='(A-45,f-10.1,A-4,f-10.1)'
    print,''
    test=choice(selection,['y','n'],'Are these units m/s? [y/n]: ',exit_statement='Exiting...')
    if strmatch(test,'y',/fold_case) then begin
       print,'*** Converting to km/s ***'
       print,' '
       velocity=velocity*1.e-3
       print,'The min and max velocity are now:', min(velocity),' and',max(velocity),'km/s',format='(A-34,f-6.1,A-5,f-6.1,A-5)'
       print,' '
    endif
  endif

  ;Test to see if in Heliocentric rest frame. 
  vel_type=FXPAR(hi_cube_head,'CTYPE3')
    helio_flag=strmatch(vel_type,'*HEL*',/fold_case)

  if error ne 0.0 then begin
    if (NOT keyword_set(magellanic)) AND (NOT keyword_set(mag2gal)) then $
    hi_cube=replicate({vel:velocity,data:fltarr(num_large),var:fltarr(num_large)+error^2.0,glon:0.0,glat:0.0},numi*numj) $
    else hi_cube=replicate({vel:velocity,data:fltarr(num_large),var:fltarr(num_large)+error^2.0,glon:0.0,glat:0.0,mlon:0.0,mlat:0.0},numi*numj)
  endif else begin
    if (NOT keyword_set(magellanic)) AND (NOT keyword_set(mag2gal)) then $
    hi_cube=replicate({vel:velocity,data:fltarr(num_large),glon:0.0,glat:0.0},numi*numj) $
    else hi_cube=replicate({vel:velocity,data:fltarr(num_large),glon:0.0,glat:0.0,mlon:0.0,mlat:0.0},numi*numj)
  endelse

endif else begin
;Image, exclude velocity

  if error ne 0.0 then begin
    if (NOT keyword_set(magellanic)) AND (NOT keyword_set(mag2gal)) then $
    hi_cube=replicate({data:0.0,var:error^2.0,glon:0.0,glat:0.0},numi*numj) $
    else hi_cube=replicate({data:0.0,var:error^2.0,glon:0.0,glat:0.0,mlon:0.0,mlat:0.0},numi*numj)
  endif else begin
    if (NOT keyword_set(magellanic)) AND (NOT keyword_set(mag2gal)) then $
    hi_cube=replicate({data:0.0,glon:0.0,glat:0.0},numi*numj) $
    else hi_cube=replicate({data:0.0,glon:0.0,glat:0.0,mlon:0.0,mlat:0.0},numi*numj)
  endelse

endelse

;Check to see if in RA and DEC
;If so, convert to header position tags
; to glon & glat reference positions and 
; delta offsets. Reconizes sin, tan, etc. 
; WCS coordinant types
glon_type=FXPAR(hi_cube_head,'CTYPE1')
  ra_flag=strmatch(glon_type,'*RA*',/fold_case)
glat_type=FXPAR(hi_cube_head,'CTYPE2')
  dec_flag=strmatch(glat_type,'*DEC*',/fold_case)
;if (ra_flag) OR (dec_flag) then $
;  HEULER, hi_cube_head, /GALACTIC

ref_glon=FXPAR(hi_cube_head,'CRVAL1')
ref_pixel_glon=FXPAR(hi_cube_head,'CRPIX1')
delta_glon=FXPAR(hi_cube_head,'CDELT1')

ref_glat=FXPAR(hi_cube_head,'CRVAL2')
ref_pixel_glat=FXPAR(hi_cube_head,'CRPIX2')
delta_glat=FXPAR(hi_cube_head,'CDELT2')

k=0.00
for i=0L, long(numi)-1 do begin
    for j=0L, long(numj)-1 do begin
        ;print,i,j,k
        hi_cube[k].data=transpose(hi_cube_survey[i,j,*])
        nodata_index=where(hi_cube[k].data eq -32768.0,count_nodata)
        if count_nodata ne 0 then hi_cube[k].data[nodata_index]=0
        xyad,hi_cube_head,i,j,glon,glat, /GALACTIC 
        ;hi_cube[k].glon=ref_glon-delta_glon*ref_pixel_glon+delta_glon*i
        ;hi_cube[k].glat=ref_glat-delta_glat*ref_pixel_glat+delta_glat*j
        hi_cube[k].glon=glon
        hi_cube[k].glat=glat

        ;Test to see if in Heliocentric rest frame. 
        ;If so, convert to the kinematic LSR rest frame.
        if keyword_set(helio_flag) then begin
          helio2lsr,velocity,vlsr,glon=hi_cube[k].glon,glat=hi_cube[k].glat
          hi_cube[k].vel=velocity
        endif

        k=k+1
    endfor
endfor

if keyword_set(magellanic) AND (NOT keyword_set(mag2gal)) then begin
   gal2mag,hi_cube.glon,hi_cube.glat,mlon,mlat
   hi_cube.mlon=mlon
   hi_cube.mlat=mlat
endif

if keyword_set(mag2gal) then begin
   hi_cube.mlon=hi_cube.glon
   hi_cube.mlat=hi_cube.mlat
   mag2gal,hi_cube.glon,hi_cube.mlat,glon,glat
   hi_cube.glon=glon
   hi_cube.glat=glat
endif

if keyword_set(k2cm2) then begin
  hi_cube.data=hi_cube.data*1.8224e18
  hi_cube.var=hi_cube.var*(1.8224e18)^2.0
endif

root=rm_ext(filename,'fits',/dash)
return_name=rm_ext(filename,'fits')

(SCOPE_VARFETCH(root,/enter,level=0))=hi_cube

save,(SCOPE_VARFETCH(root,/enter,level=0)),filename=directory+return_name+'.sav'

end