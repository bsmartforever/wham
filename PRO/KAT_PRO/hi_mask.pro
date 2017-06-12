;This is to pull out the spectra in a 1 degree radius on the hi map centered around the inputted l,b

hi_2deg=hi_data

    ComA=readobs('complex_a_iii','$HOME/WHAM/ComplexA/071107/ha_omega/',ftsext='VLSR',/ext,count=count) 
for i=0, n_elements(hi_data)-1 do begin
    hi_subindex=spectnear(hi_data,hi_data(i).glon,hi_data(i).glat,2.0)
    hi_submap=hi_data[hi_subindex]

    ;This is only used to correct headers for the sparith function
       hi_ave = REPLICATE({name:ComA(0).name, vel:fltarr(n_elements(hi_submap(0).data)),$
       data:fltarr(n_elements(hi_submap(0).data)), $  
       var:fltarr(n_elements(hi_submap(0).data))}, n_elements(hi_submap)) 

    ;This map data doesn't have have the correct header format for the sparith function, this is to fix that
    hi_ave.data=hi_submap.data
    hi_ave.vel=hi_submap.vel
    hi_ave.var=hi_submap.data*0 + 0.0389^2.0
    hi_ave=sparith(hi_ave,/ave)

    hi_2deg(i).vel=hi_ave.vel
    hi_2deg(i).data=hi_ave.data
endfor

mask=where(hi_int_2deg gt 1.0e18)

hi_int_2deg=intmap(hi_2deg,vmin=-220,vmax=-110,/hi)* 1.8224e18

mask=(hi_int_2deg gt 5.0e18)
zmin = 1.0e18 & zmax = 70.0e18

  whammap, hi_2deg, 0, 0, hi_int_2deg*mask, /useimage, $
    beamrad=0.25,smgrid = 0.5,scale = 2.0,$
    ;title = 'v!dLSR!n = -220 to -110 km s!u-1!n', $
    xtitle='Galactic Longitude [Degrees]',$
    ytitle='Galactic Latitude [Degrees]',$ 
    ymargin=[7,2], missing = missing, _extra = extra,$
    zmin = zmin, zmax = zmax, smooth = 1,/cbottom,$
    limits=[glon_min,glon_max,glat_min,glat_max],/lin,/nolabels

end