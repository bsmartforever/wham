function extract_sdss, ra, dec, radius, mass=mass, quiet=quiet

;
; Inputs:
;	radius - Radius to search for the galaxy in arcmins (minumum distance is chosen).
;			 Can be one element or the size of ra and dec. [Default: 0.5]
;
; example
; tmp=extract_sdss(229.52558,42.745842,.2,/mass)


if n_elements(radius) eq 0 then radius=fltarr(n_elements(ra))+0.5 $
else if n_elements(radius) eq 1 then radius=fltarr(n_elements(ra))+radius


if (size(ra,/type) ne 7) OR (size(dec,/type) ne 7) OR (size(radius,/type) ne 7) then begin
	ra=strcompress(string(ra),/re)
	dec=strcompress(string(dec),/re)
	radius=strcompress(string(radius),/re)
endif

;casjobs execute "SELECT  top 10 '''' + cast(p.objId as varchar(20)) + '''' as objID, p.run, p.rerun, p.camcol, p.field, p.obj, p.type, p.ra, p.dec, p.u,p.g,p.r,p.i,p.z, p.Err_u, p.Err_g, p.Err_r,p.Err_i,p.Err_z FROM fGetNearbyObjEq(164.9688,25.276,0.1) n, PhotoPrimary p WHERE n.objID=p.objID" 

cd, '$HOME/PRO/KAT_PRO/sdss/',current=proper_directory

for i=0,n_elements(ra)-1 do begin

	if (NOT keyword_set(mass)) then begin 
		command="java -jar $HOME/PRO/KAT_PRO/sdss/casjobs.jar execute " $
			+'"SELECT  top 10 '''' + cast(p.objId as varchar(20)) + '''' as ' $
			+'objID, p.run, p.rerun, p.camcol, p.field, p.obj, p.type, p.ra, p.dec,'$
			+' p.u,p.g,p.r,p.i,p.z, p.Err_u, p.Err_g, p.Err_r,p.Err_i, p.Err_z,p.petroRad_r,' $
			+' p.fiberMag_r,p.petroMag_r,p.devMag_r,p.expMag_r,p.modelMag_r,p.petroR90_r ' $
			+'FROM fGetNearbyObjEq('+ra[i]+','+dec[i]+','+radius[i]+') n, PhotoPrimary p WHERE n.objID=p.objID"'
	endif else begin
		command="java -jar $HOME/PRO/KAT_PRO/sdss/casjobs.jar execute " $
			+'"SELECT  top 10 '''' + cast(p.objId as varchar(20)) + '''' as objID, ' $
			+'p.ra, p.dec,p.petroRad_r,p.petroR90_r, ' $
			+'s.z, s.zerr, ' $
			+'passive.z as z_noqso, passive.zerr as z_noqso_err, ' $ 
			+'passive.logmass as passive_logmass, ' $ 
			+'passive.sfr as passive_sfr, ' $ 
			+'starforming.logmass as starforming_logmass, ' $ 
			+'starforming.sfr as starforming_sfr, ' $ 
			+'pca.mstellar_median as pca_logmass ' $ 
			;+'spec.specObjID as specObjID ' $ 
			;+'spec.sfr as spec_sfr ' $ 
			+'FROM fGetNearbyObjEq('+ra[i]+','+dec[i]+','+radius[i]+') n, PhotoPrimary p ' $ 
			+'Join SpecObjAll as S on P.objID = s.BestObjID ' $
			+'Join stellarMassPassivePort AS passive ON passive.specobjid = p.specobjid ' $
			+'JOIN stellarMassStarformingPort AS starforming ON passive.specobjid = starforming.specobjid ' $
			+'JOIN stellarMassPCAWiscM11 AS pca ON passive.specobjid = pca.specobjid ' $ 
			;+'JOIN galSpecExtra AS spec ON passive.specobjid = spec.specobjid ' $ 
			+'WHERE n.objID=p.objID "' 
	endelse
	spawn,command,result	
	
	if n_elements(result) ge 8 AND (NOT total(strmatch(result,'*Operation timed out*'))) then begin
		;tags 
		tmp=strsplit(result[2],'[,]',/extract)
		bad=strmatch(tmp,':*',/fold_case)
		tags=tmp[where(bad eq 0)]

		;values
		values=strsplit(result[3],'",,',/extract)

		for k=0,n_elements(values)-1 do begin
			num_test=validate_numeric(values[k])
			;This isn't the best thing to do, but it lets me avoid issues with some structures
			;having the same tags values for different galaxies with INT format sometimes and FLOATS for others. 
			if num_test eq 1 AND (NOT strmatch(tags[k],'*ID')) then values[k]=float(values[k])
		endfor

		if n_elements(sdss_str) eq 0 then begin
			str_command='structure={'+tags[0]+':'+values[0]+'}'

			junk=execute(str_command)

			add_tags, structure, tags[1:*], values[1:*],struct
			sdss_str=replicate(struct,n_elements(ra))
		endif
	endif
	for j=0, n_elements(tags)-1 do sdss_str[i].(j)=values[j]
endfor

;CD back into the original directory
cd,proper_directory

if n_elements(sdss_str) eq 0 AND (NOT keyword_set(quiet)) then begin
	print,''
	print,'*** No matches found ***'
	print,''
	
endif

if n_elements(sdss_str) eq 0 then return,0

return,sdss_str

end