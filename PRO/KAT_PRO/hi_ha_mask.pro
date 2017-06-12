function hi_ha_mask,ca_data,line_data,cutoff,velocity,radius=radius

hi=1

vmin=min(velocity)
vmax=max(velocity)

hi_mask=fltarr(n_elements(ca_data))
for i=0, n_elements(ca_data)-1 do begin
    line_subindex=spectnear(line_data,ca_data(i).glon,ca_data(i).glat,radius)
    line_submap=line_data[line_subindex]

    ;This is only used to correct headers for the sparith function
           line_ave = REPLICATE({vel:fltarr(n_elements(line_data(0).vel)),$
           data:fltarr(n_elements(line_data(0).data)), $  
           var:fltarr(n_elements(line_data(0).data)),$
	   glon:1.0,$
	   glat:1.0}, n_elements(line_submap)) 

    ;This map data doesn't have have the correct header format for the sparith function, this is to fix that
    line_ave.data=line_submap.data
    line_ave.vel=line_submap.vel
    if keyword_set(hi) then line_ave.var=line_submap.data*0 + 0.0389^2.0 else line_ave.var=line_submap.var
    line_ave.glon=line_submap.glon
    line_ave.glat=line_submap.glat
    line_ave=sparith(line_ave,/ave)

    temp=int_tabulated(line_ave.vel(where((line_ave.vel gt vmin) and (line_ave.vel lt vmax))),line_ave.data(where((line_ave.vel gt vmin) and (line_ave.vel lt vmax))))* 1.8224e18

    hi_mask(i)=(temp gt cutoff)

endfor

   return,hi_mask;

end