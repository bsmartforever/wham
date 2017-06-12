function read_galex, xpos, ypos, radius, radec=radec, hour=hour, degree=degree

; Purpose:
;	Extract the name, RA (decimal hours), DEC, and NUV/FUV fluxes of each 
;	target in the GALEX GR7 and catalogs into a structure. 
;
;	The locations of each caracter in the tables is found in the 
;	ReadMe document.
;
; Input:
;  xpos/ypos - Target coordinants. [Default Galactic Coordinants]
;  radius    - Search radius for background objects [Default 1 degree]
;
;  radec     - Specifies that xpos/ypos are in ra and dec. 
;           [Default ra units in hours]
;  degrees   - Sets ra units to degrees if radec is passed
;
;	Catalog website (select tar):
; 	http://cdsarc.u-strasbg.fr/viz-bin/Cat?VII/258
;
; By Dr. Kat Barger 04/2014 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;If radec is passed, then assume that glon=ra and glat=dec
;Note: the default is to assume that ra is in hours.
if keyword_set(radec) then glactc,xpos,ypos,2000,glon,glat,1,degree=degree $
else begin 
   glon=xpos & glat=ypos 
endelse 

;Assume that the radius is 1 degree if not otherwise stated. 
if (NOT keyword_set(radius)) then radius=1.

glat_in=glat

again:

case 1 of
	glat gt  80:   file='glat80_90N.sav'
	glat gt  70:   file='glat70_80N.sav'
	glat gt  60:   file='glat60_70N.sav'
	glat gt  50:   file='glat50_60N.sav'
	glat gt  40:   file='glat40_50N.sav'
	glat gt  30:   file='glat30_40N.sav'
	glat gt  20:   file='glat20_30N.sav'
	glat gt  10:   file='glat10_20N.sav'
	glat gt  00:   file='glat00_10N.sav'	
	glat gt -10:   file='glat10_00S.sav'
	glat gt -20:   file='glat20_10S.sav'
	glat gt -30:   file='glat30_20S.sav'
	glat gt -40:   file='glat40_30S.sav'
	glat gt -50:   file='glat50_40S.sav'
	glat gt -60:   file='glat60_50S.sav'
	glat gt -70:   file='glat70_60S.sav'
	glat gt -80:   file='glat80_70S.sav'
	glat gt -90:   file='glat90_80S.sav'
	else:    file='??'
endcase

dir='/d/data/GALEX/'
;struct is the variable name saved in these files. 
restore,dir+file

loc=spectnear(struct,glon,glat_in,radius,count,dist = dist)
if (count NE 0) AND (n_elements(galex) eq 0) then galex=struct[loc] else $
if (count NE 0) AND (n_elements(galex) ne 0) then $
	galex=[galex,struct[loc]]

glat=glat_in

;This is to grab those sightlines that teeder between two different lat regions.
if glat+radius le (floor(glat/10.)-1.)*10. AND (n_elements(flag_low) eq 0) then begin
	glat=glat_in-10.
	flag_low=1
	goto, again
endif
if glat+radius ge (ceil(glat/10.))*10. AND (n_elements(flag_high) eq 0) then begin
	glat=glat_in+10.
	flag_high=1
	goto, again
endif

;if n_elements(galex) ne 0 then begin
;	good_uv=where((galex.fuv ne -999.0) AND (galex.nuv ne -999.0),count)
;	if count ne 0 then galex=galex[good_uv]
;endif

pause
if n_elements(galex) eq 0 then begin
	print,''
	print,'*** No Sightlines in specified region'
	print,''
	return,0
endif else return,galex

end