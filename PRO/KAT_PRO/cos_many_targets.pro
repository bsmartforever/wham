function cos_many_targets, xpos_arr, ypos_arr, radius, $
	vmag=vmag, sn=sn, $
	tolerance=tolerance,$
	radec=radec, degree=degree


;
;
;
;
;
; Created by Dr. Kat Barger 03/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

for i=0, n_elements(xpos_arr)-1 do begin
	print,i
	tmp=cos_targets(xpos_arr[i],ypos_arr[i],radius,radec=radec,degree=degree,$
		vmag=vmag,tolerance=tolerance,/noplot,/quiet)
	help,tmp
	if (size(tmp,/type) eq 8) then help,tmp,/str else help,tmp
	if (size(tmp,/type) eq 8) AND (size(targets,/type) eq 0) then targets=tmp $
	else if (size(tmp,/type) eq 8) then targets=[targets,tmp]

endfor
	
	if (size(targets,/type) eq 8) then return,targets $
	else return,0

end