function coadd, str1, str2, plot=plot

; Purpose:
;	To coadd two spectra. Their velocties can span different ranges or overlap.
;
; Input:
;	str1 - spectrum structure containing vel, data, var, glon, glat tags
;	str2 - same as str1
;
; Output:
;	combined spectra containing same tags as str1 & str2, but also with name=' '
;
; Example:
;	new_str=coadd(str1,str2)
;
; Created by Dr. Kat Barger 05/2014
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

v_min=min([str1.vel,str2.vel],max=v_max)
vbin=round(max([(max(str1.vel)-min(str1.vel))/n_elements(str1.vel),$
		        (max(str2.vel)-min(str2.vel))/n_elements(str2.vel)]))

vel_arr=makearr((v_max-v_min)/float(vbin),v_min,v_max)

 new_str={name:' ',vel:vel_arr,data:fltarr(n_elements(vel_arr)),var:fltarr(n_elements(vel_arr)),$
 		  glon:str1.glon,glat:str1.glat}

 new_str1_data=interpol(str1.data,str1.vel,vel_arr)
          zero=where((vel_arr lt min(str1.vel)) or (vel_arr gt max(str1.vel)),nzero)
          if nzero ne 0 then new_str1_data(zero)=0
          ;Calculate error for the specified velocity binning. 
          var1=(linear_interpol_err(str1.vel,vel_arr,str1.data,sqrt(str1.var)))^2.        

 new_str2_data=interpol(str2.data,str2.vel,vel_arr)
          zero=where((vel_arr lt min(str2.vel)) or (vel_arr gt max(str2.vel)),nzero)
          if nzero ne 0 then new_str2_data(zero)=0
          ;Calculate error for the specified velocity binning. 
          var2=(linear_interpol_err(str2.vel,vel_arr,str2.data,sqrt(str2.var)))^2.      

new_str.data=(new_str1_data+new_str2_data)/2.
new_str.var=(var1+var2)/2.^2.

if keyword_set(plot) then begin
	ploterror,str1.vel,str1.data,sqrt(str1.var),xrange=[v_min,v_max]
	oploterror,str2.vel,str2.data,sqrt(str2.var)              
	oploterror,new_str.vel,new_str.data,sqrt(new_str.var),color=fsc_color('green'),errcolor=fsc_color('green')
endif

return,new_str

end