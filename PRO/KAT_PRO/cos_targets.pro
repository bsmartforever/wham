function cos_targets, xpos, ypos, radius, $
	vmag=vmag, $ ;orbits=orbits, sn=sn, $
	tolerance=tolerance,$
	radec=radec, degree=degree,$
	noplot=noplot,quiet=quiet

; Purpose:
;	Search for potential COS observation targets by cross 
;	correlating QSO/AGN catalogs with bright GALEX sightlines. 
;
; Input:
;	xpos/ypos - Target coordinants. [Default Galactic Coordinants]
;	radius    - Search radius for background objects [Default 1 degree]
;	vmag      - Minimum V magnitude for QSO/AGN targets [Default 17.5 mag]
;	tolerance - Maximum angular distance for an AGN/QSO+GALEX match 
;				in degrees [Default 0.05 an arcmin]
;	radec     - Specifies that xpos/ypos are in ra and dec. 
;				[Default ra units in hours]
;   degrees   - Sets ra units to degrees if radec is passed
;
; Output: Structure containing both QSO/AGN and GALEX sightline information.
;
; Example:
;	IDL>target=cos_targets(16.08125,35.57000,0.06,/radec,/degree,vmag=20)
; 	IDL> help,target,/str
;	** Structure <e403768>, 15 tags, length=112, data length=106, refs=1:
;	   NAME            STRING    'NGC  266         '
;	   RA              DOUBLE           12.450000
;	   DEC             DOUBLE           32.277778
;	   GLON            DOUBLE           122.52971
;	   GLAT            DOUBLE          -30.592589
;	   VMAG            FLOAT           13.0600
;	   Z               FLOAT         0.0150000
;	   GALEXID         INT          10297
;	   DSTARCMIN            DOUBLE         0.038625239  ; DisArcMin = distance in arcmins
;	   RA_GALEX        DOUBLE           12.449233
;	   DEC_GALEX       DOUBLE           32.277692
;	   GLON_GALEX      DOUBLE           122.52910
;	   GLAT_GALEX      DOUBLE          -30.592770
;	   FUV             FLOAT           427.368
;	   NUV             FLOAT          -999.000
;	
;
; Created by Dr. Kat Barger 03/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;If radec is passed, then assume that glon=ra and glat=dec
;Note: the default is to assume that ra is in hours.
if keyword_set(radec) then glactc,xpos,ypos,2000,glon,glat,1,degree=degree $
else begin 
   glon=xpos & glat=ypos 
endelse 

;Set to 1 degree by default
if (NOT keyword_set(radius)) then radius=1.

;Search the Vernon & Vernon catalogue
if (NOT keyword_set(vmag)) then vmag = 17.5
if (NOT keyword_set(qso)) AND (NOT keyword_set(agn)) then begin
	qso=1 & agn=1 
endif

if (NOT keyword_set(quiet)) then print,'** Searching for QSOs...'
target_qso=read_vv(glon, glat, radius, vmag=vmag, quiet=quiet)

if (size(target_qso,/type) ne 8) then begin
	print,''
	print,'*** No Vernon & Veron sightlines meet your criteria'
	print,''
	return,0
endif

;;;[GALEX];;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This only looks for GALEX targets near located QSOs and AGNs.
;This is done because the amount of memory needed to extract all 
;GALEX targets within the global searched region is way too large 
;for my memory allotment on the casjobs website. 

if (NOT keyword_set(quiet)) then print,'** Searching GALEX...'
if (NOT keyword_set(tolerance)) then tolerance=0.05
dist_qso=fltarr(n_elements(target_qso))

undefine,target_galex
bad=fltarr(n_elements(target_qso))
for index=0,n_elements(target_qso)-1 do begin

	;Search GALEX databases. 
	tmp=extract_galex(target_qso[index].glon, target_qso[index].glat, tolerance/60., quiet=quiet)

	if (size(tmp,/type) eq 8) then begin 

		dist_galex=fltarr(n_elements(tmp))
    	dist_galex=sphdist(target_qso[index].glon, target_qso[index].glat, tmp.glon, tmp.glat,/double)
	
    	junk=min(dist_galex,loc)
    	dist_qso[index]=dist_galex[loc]
    	;Only select the nearest GALEX source to the QSO/AGN
    	tmp=tmp[loc]

    	if (size(target_galex,/type) eq 0) then target_galex=tmp $
		else  if (size(tmp,/type) eq 8) then target_galex=[target_galex,tmp]
		bad[index]=0.

    endif else bad[index]=1

endfor
badloc=where(bad ne 1,nbad)
if nbad ne n_elements(target_galex) then begin
	print,'Not equal'
	pause
	read,'kill me',die

endif

if nbad ne 0 then target_qso=target_qso[badloc]

;Convert micro Janskys to (erg cm^-2 s^-1 Angstrom^-1)
;target_galex.fuv_flux=galex2hst(flux)
;This is cutting out those that need orbits > 25 for SN=10 with G130M+G160M
if size(target_galex,/type) eq 8 then begin
	;There might be a bug somewhere that sometimes results in '*_flux' tags instead of f/nuv.
	;Either way, this will perminantly eliminate this issue as the original sturcture is returned if 
	;the pesky tag is absent
	target_galex=rename_tags(target_galex,['fuv_flux','nuv_flux'],['fuv','nuv'])
	good_flux=where((galex2hst(target_galex.fuv) gt 5.0e-17) OR (galex2hst(target_galex.nuv) gt 5.0e-17),count) 
endif else count=0
if (count eq 0) then begin
	if (NOT keyword_set(quiet)) then begin
		print,''
		print,'*** No GALEX sightlines in specified region'
		print,''
	endif
	return,0
endif else begin
	target_galex=target_galex[good_flux]
	target_qso=target_qso[good_flux]
endelse

if (NOT keyword_set(noplot)) then begin
	if keyword_set(radec) then begin
		xtag='ra' & ytag='dec'
		xtitle='RA (Degrees)' & ytitle='DEC (Degrees)'
	endif else begin
		xtag='glon' & ytag='glat'
		xtitle='Galactic Longitude (Degrees)' & ytitle='Galactic Latitude (Degrees)'
	endelse

	xmin=min([min(target_galex.(tag_loc(target_galex,xtag))),xpos,min(target_qso.(tag_loc(target_qso,xtag)))])
	xmax=max([max(target_galex.(tag_loc(target_galex,xtag))),xpos,max(target_qso.(tag_loc(target_qso,xtag)))])
	ymin=min([min(target_galex.(tag_loc(target_galex,ytag))),ypos,min(target_qso.(tag_loc(target_qso,ytag)))])
	ymax=max([max(target_galex.(tag_loc(target_galex,ytag))),ypos,max(target_qso.(tag_loc(target_qso,ytag)))])
	if (xmin-xmax) lt 5./60. then begin
		xmin=xmin-2.5/60. & xmax=xmax+2.5/60.
	endif
	if (ymin-ymax) lt 5./60. then begin
		ymin=ymin-2.5/60. & ymax=ymax+2.5/60.
	endif

	plot,[1,2,3],/nodata,$
		xrange=[xmin,xmax],yrange=[ymin,ymax],xstyle=3,ystyle=3,charsize=1.5,charthick=1,$
		psym=symcat(16),symsize=1.25,xtitle=xtitle,ytitle=ytitle
	plots,target_galex.(tag_loc(target_galex,xtag)),target_galex.(tag_loc(target_galex,ytag)),$
		psym=symcat(16),symsize=1.25,color=fsc_color('orange')
	plots,xpos,ypos,color=fsc_color('blk5'),psym=symcat(9),symsize=4,thick=3
	plots,target_qso.(tag_loc(target_qso,xtag)),target_qso.(tag_loc(target_qso,ytag)),$
		color=fsc_color('green'),psym=1,symsize=1.5


	if (NOT keyword_set(quiet)) then begin 
    	print,''
    	print,'** Targeted location       Grey Circle'
    	print,'** QSOs or AGN sightlines  Green Crosses'
    	print,'** QSO/AGN+GALEX Matches   Orange Dots'
    	print,''
    endif

endif

	if n_elements(target_qso) ne n_elements(target_galex) then begin
		pause
		print,'kill me',die
	endif

	target_galex=rename_tags(target_galex,['objid','ra','dec','glon','glat','nuv_flux','fuv_flux'],$
		['galexid','ra_galex','dec_galex','glon_galex','glat_galex','nuv','fuv'])
	combine_structs,target_qso,target_galex,matches 

	;near=where(matches.dstarcmin le tolerance,count)

	if count ne 0 then return,matches

end