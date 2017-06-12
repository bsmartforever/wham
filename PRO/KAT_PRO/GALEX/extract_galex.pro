function extract_galex, xpos, ypos, radius, $
	radec=radec, degree=degree, quiet=quiet

; Purpose:
;	Extract the name, RA (decimal hours), DEC, and Vmag of each 
;	target in the GALEX catalogs into a structure directly from the 
;	MAST archive online. MUST BE CONNECTED TO THE INTERNET! 
;	Searches the GR6+7 and GALEXCatalogs for targets with 
;	either NUV or FUV.
;
; Input:
;  xpos/ypos - Target coordinants. [Default Galactic Coordinants]
;  radius    - Search radius in degrees for background objects [Default 1 degree]
;
;  radec     - Specifies that xpos/ypos are in ra and dec. 
;              [Default ra units in hours]
;  degrees   - Sets ra units to degrees if radec is passed
;
; Output:
;	Structure containing the objid, ra (degrees), dec (degrees), glon, glat, vmag (mag), FUV (mag), NUV (mag)
;
; Example:
;  tmp=extract_galex(14.97208,31.82694,0.06,/radec,/degree)
;
;
;
; Restrictions:
;	Computer MUST be connected to the internet as the GALEX catalogs 
;	are searched in real time. This is because there are many UV bright 
;	targets that are not listed in the default FUV/NUV catalog (GR6+7). 
;	To conduct these searches, the terminal interface "casjobs" is used.
;
; By Dr. Kat Barger 04/2014 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if (n_elements(xpos) eq 0) OR (n_elements(ypos) eq 0) then begin
	print,''
	print,'** Neither xpos nor ypos are defined.'
	print,'** Please specify the targeted region'
	print,''
	return,0
endif

;If radec is passed, then assume that glon=ra and glat=dec
;Note: the default is to assume that ra is in hours.
if keyword_set(radec) then begin 
	ra=xpos & dec=ypos
	glactc,xpos,ypos,2000,glon,glat,1,degree=degree
endif else begin 
   glon=xpos & glat=ypos 
   glactc,ra,dec,2000,glon,glat,2,/degree
endelse 

   ;Assume that the radius is 1 degree if not otherwise stated. 
   if (NOT keyword_set(radius)) then radius=1.
   if radius gt 0.1 then large=1
large=1 ;hardcoding in large for now, apparently there is an issue somewhere
x1=glon
y1=glat
y2=glat
glon_diff=acos((cos(radius*!DDEGRA)-sin(y1*!DDEGRA)*sin(y2*!DDEGRA))/(cos(y1*!DDEGRA)*cos(y2*!DDEGRA)))*!DRADEG

glon_min=glon-abs(glon_diff) mod 360.
glon_max=glon+abs(glon_diff) mod 360.
glat_min=glat-radius
glat_max=glat+radius

glactc,ra_arr,dec_arr,2000,[glon_min,glon_max],[glat_min,glat_max],2,/degree

ra_min=min(ra_arr)
ra_max=max(ra_arr)
dec_min=min(dec_arr)
dec_max=max(dec_arr)

radius_arcmin=radius*60.

if (keyword_set(large)) then begin

	cd, '$HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/',current=proper_directory
	;Apparently the execute command is useful for quick and dirty searches, but submit is prefered for larger searches. 
	;However, submit assigns the job and id and I have no idea how to extract that data or how to check on it
	;casjobs run -t "GALEXGR6Plus7" -n "test query" "select top 10 ra into mydb.test0 from photoobj"

	;First make sure that the directory and the casjobs are clear of previous GALEX searches
	spawn,'rm -f galex_catalogs_kbarger*.csv'
	spawn,'rm -f galex_gr6plux7_kbarger*.csv'
	
	command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar execute -t 'MyDB' -n 'drop query' 'drop table galex_catalogs'")
	spawn,command,result
	command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar execute -t 'MyDB' -n 'drop query' 'drop table galex_gr6plux7'")
	spawn,command,result
	;End of house cleaning
	
	command= $
		strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar run -t 'GALEX_Catalogs' -n 'test query' " + $ 
	    "'select p.objid, nb.distance as dstArcMin, p.ra, p.dec, p.glon, p.glat, p.fuv_flux as fuv, p.nuv_flux as nuv into mydb.galex_catalogs from bcscat_ais as p,"+ $
	    " fgetnearbyobjeq("+string(ra)+","+string(dec)+","+string(radius_arcmin)+"/*arcmins*/) as nb "+ $
	    "where p.ra BETWEEN "+string(ra_min)+" AND "+string(ra_max)+" AND p.dec BETWEEN "+string(dec_min)+" AND "+string(dec_max)+ $
	    "AND ((p.fuv_mag > -99 and p.fuv_mag < 99) OR (p.nuv_mag > -99 and p.nuv_mag < 99))"+ $ 
	    " order by dstArcMin' ")
	
	spawn,command,result
;pause
	rows=fix((strsplit(result(n_elements(result)-2),':',/extract))[3])
	
	;Create a fits file if matches found in GALEXCatalogs
	if rows ne 0 then begin
		command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar extract -b galex_catalogs -F -d -r")
		spawn,command,result
	
		;While the csv file is empty, mindlessly loop until populated. I don't think that the "-r" command above for the
		;casjobs command is working, which should cause the program to wait until the comamon is done being executed.
		;Consider implementing this. Note that it would be obnoxious to trouble shoot as it's an indefinate loop if 
		;the csv file is never populated.
		check=''
		while check eq ''  do spawn,'find . -type f -size +1c |grep -i csv',check,error
		tmp=read_csv('galex_catalogs_kbarger*.csv')
		;For some crazy reason, the code makes it to here even though the csv file contains no data...
		;The nrows check should account for this, but it's not for some reason...
		if size(tmp,/type) eq 8 then begin
			remove_tags,tmp,['nrows','ncols'],struct_galex
			struct_galex=struct_conv(struct_galex)
		endif
		spawn,'rm -f galex_catalogs_kbarger*.csv'
	endif
	command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar execute -t 'MyDB' -n 'drop query' 'drop table galex_catalogs'")
	spawn,command,result
	;command= $
	;	strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar execute "+ $ 
	;    "'select p.objid, nb.distance as dstArcMin, p.ra, p.dec, p.glon,p.glat, p.fuv_flux as fuv, p.nuv_flux as nuv from bcscat_ais as p,"+ $
	;    " fgetnearbyobjeq("+string(ra)+","+string(dec)+","+string(radius_arcmin)+"/*arcmins*/) as nb "+ $
	;    "where p.ra BETWEEN "+string(ra_min)+" AND "+string(ra_max)+" AND p.dec BETWEEN "+string(dec_min)+" AND "+string(dec_max)+ $
	;    "AND ((p.fuv_mag > -99 and p.fuv_mag < 99) OR (p.nuv_mag > -99 and p.nuv_mag < 99))"+ $ 
	;    " order by dstArcMin'")
	
	;Call the casjobs from the xterminal
	;spawn,command,result
	
	;if n_elements(result) gt 8 then begin
	;	result=result[3:n_elements(result)-5]
	;	
	;	tmp=strsplit(result[3],',',/extract)
	;	struct_galex=replicate({id:0,dist:0.0D,ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,fuv:0.0,nuv:0.0},n_elements(result))
	;	struct_galex=struct_conv(struct_galex)
	;	
	;	for i=0,n_elements(result)-1 do begin
	;		tmp=strsplit(result[i],',',/extract)
	;		struct_galex[i].id=fix(tmp[0],type=3)
	;		struct_galex[i].dstarcmin=double(tmp[1])
	;		struct_galex[i].ra=double(tmp[2])
	;		struct_galex[i].dec=double(tmp[3])
	;		struct_galex[i].glon=double(tmp[4])
	;		struct_galex[i].glat=double(tmp[5])
	;		struct_galex[i].fuv=float(tmp[6])
	;		struct_galex[i].nuv=float(tmp[7])
	;	endfor
	;endif
	
	;To load the GALEXGR6+7. Must CD into the directory below for the casjobs to work propertly.
	;cd, '$HOME/PRO/KAT_PRO/GALEX/GALEXGR6Plus7'
	
	command= $
		strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar run -t 'GALEX_GR6Plus7' -n 'test query' " + $ 
		" 'select nb.objid,nb.distance as dstArcMin,p.ra,p.dec,p.glon,p.glat,p.nuv_flux,p.fuv_flux into mydb.galex_gr6plux7 "+$
		"from fgetnearbyobjeq("+string(ra)+","+string(dec)+","+string(radius_arcmin)+"/*arcmins*/) as nb, photoobjall as p, "+$
		" photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ "+$
	    "AND ((p.fuv_mag > -99 and p.fuv_mag < 99) OR (p.nuv_mag > -99 and p.nuv_mag < 99))"+ $ 
		" order by dstArcMin' ")
	
	;command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXGR6Plus7/casjobs.jar execute"+$
	;	" 'select nb.objid,nb.distance as dstArcMin,p.ra,p.dec,p.glon,p.glat,p.nuv_flux,p.fuv_flux "+$
	;	"from fgetnearbyobjeq("+string(ra)+","+string(dec)+","+string(radius_arcmin)+"/*arcmins*/) as nb, photoobjall as p, "+$
	;	" photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ "+$
	;    "AND ((p.fuv_mag > -99 and p.fuv_mag < 99) OR (p.nuv_mag > -99 and p.nuv_mag < 99))"+ $ 
	;	" order by dstArcMin'")
	;Call the casjobs from the xterminal
	spawn,command,result
	
	rows=fix((strsplit(result(n_elements(result)-2),':',/extract))[3])
	
	if rows ne 0 then begin
		command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar extract -b galex_gr6plux7 -F -d -r")
		spawn,command,result
		check=''
		while check eq ''  do spawn,'find . -type f -size +1c |grep -i csv',check,error
		tmp=read_csv('galex_gr6plux7_kbarger*.csv')
		;For some crazy reason, the code makes it to here even though the csv file contains no data...
		;The nrows check should account for this, but it's not for some reason...
		if size(tmp,/type) eq 8 then begin
			remove_tags,tmp,['nrows','ncols'],struct_gr6p7
			struct_gr6p7=struct_conv(struct_gr6p7)
		endif
		spawn,'rm -f galex_gr6plux7_kbarger*.csv'
	endif
	command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar execute -t 'MyDB' -n 'drop query' 'drop table galex_gr6plux7'")
	spawn,command,result

endif else if (NOT keyword_set(large)) then begin

	;To load the GALEXCatalogs. Must CD into the directory below for the casjobs to work propertly.
	cd, '$HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs',current=proper_directory

	command= $
		strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXCatalogs/casjobs.jar execute "+ $ 
	    "'select p.objid, nb.distance as dstArcMin, p.ra, p.dec, p.glon,p.glat, p.fuv_flux as fuv, p.nuv_flux as nuv from bcscat_ais as p,"+ $
	    " fgetnearbyobjeq("+string(ra)+","+string(dec)+","+string(radius_arcmin)+"/*arcmins*/) as nb "+ $
	    "where p.ra BETWEEN "+string(ra_min)+" AND "+string(ra_max)+" AND p.dec BETWEEN "+string(dec_min)+" AND "+string(dec_max)+ $
	    "AND ((p.fuv_mag > -99 and p.fuv_mag < 99) OR (p.nuv_mag > -99 and p.nuv_mag < 99))"+ $ 
	    " order by dstArcMin'")
	
	;Call the casjobs from the xterminal
	spawn,command,result

	if n_elements(result) gt 8 then begin

		result=result[3:n_elements(result)-5]
		struct_galex=replicate({objid:0LL,dstarcmin:0.0D,ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,fuv_flux:0.0,nuv_flux:0.0},n_elements(result))
		struct_galex=struct_conv(struct_galex)
		
		for i=0,n_elements(result)-1 do begin
			tmp=strsplit(result[i],',',/extract)
			struct_galex[i].objid=fix(tmp[0],type=14)
			struct_galex[i].dstarcmin=double(tmp[1])
			struct_galex[i].ra=double(tmp[2])
			struct_galex[i].dec=double(tmp[3])
			struct_galex[i].glon=double(tmp[4])
			struct_galex[i].glat=double(tmp[5])
			struct_galex[i].fuv_flux=float(tmp[6])
			struct_galex[i].nuv_flux=float(tmp[7])
		endfor
	endif
	
	;To load the GALEXGR6+7. Must CD into the directory below for the casjobs to work propertly.
	cd, '$HOME/PRO/KAT_PRO/GALEX/GALEXGR6Plus7'

	command=strcompress("java -jar $HOME/PRO/KAT_PRO/GALEX/GALEXGR6Plus7/casjobs.jar execute"+$
		" 'select nb.objid,nb.distance as dstArcMin,p.ra,p.dec,p.glon,p.glat,p.nuv_flux,p.fuv_flux "+$
		"from fgetnearbyobjeq("+string(ra)+","+string(dec)+","+string(radius_arcmin)+"/*arcmins*/) as nb, photoobjall as p, "+$
		" photoextract as pe where nb.objid=p.objid and p.photoextractid=pe.photoextractid and p.band=1 /* band = NUV */ "+$
	    "AND ((p.fuv_mag > -99 and p.fuv_mag < 99) OR (p.nuv_mag > -99 and p.nuv_mag < 99))"+ $ 
		" order by dstArcMin'")

	;Call the casjobs from the xterminal
	spawn,command,result

	if n_elements(result) gt 8 then begin
	
		result=result[3:n_elements(result)-5]
		struct_gr7=replicate({objid:0LL,dstarcmin:0.0D,ra:0.0D,dec:0.0D,glon:0.0D,glat:0.0D,fuv_flux:0.0,nuv_flux:0.0},n_elements(result))
		struct_gr7=struct_conv(struct_gr7)
		
		for i=0,n_elements(result)-1 do begin
			tmp=strsplit(result[i],',',/extract)
			struct_gr7[i].objid=fix(tmp[0],type=14)
			struct_gr7[i].dstarcmin=double(tmp[1])
			struct_gr7[i].ra=double(tmp[2])
			struct_gr7[i].dec=double(tmp[3])
			struct_gr7[i].glon=double(tmp[4])
			struct_gr7[i].glat=double(tmp[5])
			struct_gr7[i].fuv_flux=float(tmp[6])
			struct_gr7[i].nuv_flux=float(tmp[7])
		endfor
	endif
endif

;CD back into the original directory
cd,proper_directory

	
if (size(struct_galex,/type) eq 8) AND (size(struct_gr6p7,/type) eq 8) then begin
	struct=[struct_galex,struct_gr6p7] 
	struct=struct[rem_dup(struct.objid)]
	struct=struct[sort(struct.DSTARCMIN)]
endif else if (size(struct_galex,/type) eq 8) then struct=struct_galex $
else if (size(struct_gr6p7,/type) eq 8) then struct=struct_gr6p7 $
else begin
	if (NOT keyword_set(quiet)) then begin
		print,''
		print,'** No GALEX targets found'
		print,''
	endif
	return,0
endelse

struct=struct[rem_dup(struct.objid)]
return, struct


;	if (n_elements(struct_galex)) ne 0 then begin
;		struct=[struct_galex,struct_gr7] 
;		struct=struct[sort(struct.dist)]
;		return,struct
;	endif else return,struct_gr7

;endif else begin


;endelse

end