function mask,line_data,cutoff,velocity,radius=radius,hi=hi,ha=ha,inverse=inverse,index=index

; Purpose: 
;	This function produces a mask to cutoff the faint emission from a map.
;	This is accomplished by first constructing a smoothed map by increasing 
;	the beam size by a factor or 2 or more. The resulting soothed map will 
;	have the same number of elements as the original map, but each sightline 
;	will now be a averaged of its nearby neighbors. Then the background cutoff 
;	is applied this map to produce a mask.
; 
; Input:
; 	line_data = Structure containing vel, data, var, glon, glat information.
;	cutoff = Excludes values in the map below this value. Expects Ha cutoff 
;            in units of R and HI in units of column density.
;	velocity = [vmin,vmax] array
;	radius =  Radius of smoothed beam. Default twice the standard beamsize: 
;             0.5 for hi and 1.0 for ha.
;	hi and ha = Specify the emission, must specify one or the other. This is used
;             to determine the variance (assumes LAB data) and to determine the 
;             scaling factor required to convert to R or column densities. 
;             NOTE: The /ha keyword works with all mask lines. 
;	[inverse] = Provides the inverse of the mask, masks intensities above
;             cutoff value. [optional]
;	[index] = Returns the indices of the values below the cutoff instead of a mask.
;             Works with [inverse] to return indices above the cutoff. [optional]
;
; Output:
;	An array of size n_elements(line_data) that contains 1s & 0s for masking emission below
;       (or above with /inverse) a specified intensity or column density cutoff value. The 
;       output is optimized to be used with when mapping the data with whammap, 
;       i.e., whammap,data,0,0,intensity*mask,/useimage. The funciton can also return the 
;       indices of the values below (or above with /inverse) the cutoff value with /index 
;       if desired. 
;
;   e.g., mask_of_values_below_cutoff=mask(hi_data,5.0e18,[-220,-110],/hi)
;         indices_of_values_below_cutoff=mask(ha_data,0.06,[100,300],radius=.6,/ha,/inverse,/index)
;
; Created by Dr. Kat Barger
; Modified Dec 2012 to include inverse and index keywords and to fix a *.glon & *.glat bug 
;          with the expected input of spectnear.
;

IF (NOT keyword_set(hi)) and (NOT keyword_set(ha)) THEN begin
   print,'***********************************'
   print,'*** Hi or Ha MUST be specified! ***'
   print,'***********************************'
   return,line_data.data*0+1;
ENDIF

IF (NOT keyword_set(radius)) and (keyword_set(hi)) then radius =  0.5
IF (NOT keyword_set(radius)) and (keyword_set(ha)) then radius =  1.0

IF cutoff lt min(line_data.data) then begin
   print,'*******************************************'
   print,'*** Cutoff is less then min data value. ***'
   print,'***         No mask produced            ***'
   print,'*******************************************'
   return,line_data.data*0+1;
endif


vmin=min(velocity)
vmax=max(velocity)

line_2deg=line_data
for i=0, n_elements(line_data)-1 do begin
    line_subindex=spectnear(line_data,line_data(i).glon,line_data(i).glat,radius)
    line_submap=line_data[line_subindex]

    ;This is only used to correct headers for the sparith function
           line_ave = REPLICATE({vel:fltarr(n_elements(line_data(0).vel)),$
           data:fltarr(n_elements(line_data(0).data)), $  
           var:fltarr(n_elements(line_data(0).data)),glon:1.0,glat:1.0}, n_elements(line_submap)) 

    ;This map data doesn't have have the correct header format for the sparith function, this is to fix that
    line_ave.data=line_submap.data
    line_ave.vel=line_submap.vel
    line_ave.glon=line_submap.glon
    line_ave.glat=line_submap.glat
    if keyword_set(hi) then line_ave.var=line_submap.data*0 + 0.0389^2.0 else line_ave.var=line_submap.var
    line_ave=sparith(line_ave,/ave)

    line_2deg(i).vel=line_ave.vel[0:n_elements(line_data(i).vel)-1]
    line_2deg(i).data=line_ave.data[0:n_elements(line_data(i).data)-1]
endfor

IF keyword_set(hi) then line_int_2deg=intmap(line_2deg,vmin=vmin,vmax=vmax,/hi)* 1.8224e18
IF keyword_set(ha) then line_int_2deg=intmap(line_2deg,vmin=vmin,vmax=vmax)* 1.0/22.8

IF keyword_set(ha) then begin
   line_int_2deg=fltarr(n_elements(line_2deg))

   for i=0, n_elements(line_2deg)-1 do begin
       vgood=where((line_2deg(i).vel ge vmin) and (line_2deg(i).vel le vmax))
       line_int_2deg(i)=int_tabulated(line_2deg(i).vel(vgood),line_2deg(i).data(vgood))/22.8
   endfor
endif

mask=(line_int_2deg gt cutoff)
if Keyword_set(index) then mask=where(line_int_2deg gt cutoff)

if Keyword_set(inverse) then mask=(line_int_2deg lt cutoff)
if Keyword_set(inverse) and Keyword_set(index) then mask=where(line_int_2deg lt cutoff)

    return, mask;
end