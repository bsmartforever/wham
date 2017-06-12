function hi_spectra, glon, glat, radius=radius, save=save, spe=spe, hi_data=hi_data,no_ave=no_ave, num=num, dir=dir

if (NOT keyword_set(dir)) then dir='/d/data/hi/LAB/'

if (NOT keyword_set(radius)) then radius=0.55

if (NOT keyword_set(glon)) or (NOT keyword_set(glat)) then begin
    print,''
    print,'*** Please specify glon and glat. ***'
    print,'USAGE: function hi_spectra, glon, glat, radius=radius, save=save, spe=spe, hi_data=hi_data,no_ave=no_ave'
    print,''
    return,0
endif

num=n_elements(glon)

;convert glat and glon to float arrays.
zero_arr=fltarr(num)
glon=zero_arr+glon
glat=zero_arr+glat

var_flag=0
orig_radius=radius
for i=0, num-1 do begin

   glon_min=glon[i]-radius-5.0
   glon_max=glon[i]+radius+5.0
   glat_min=glat[i]-radius-5.0
   glat_max=glat[i]+radius+5.0

   if (NOT keyword_set(hi_data)) then begin
      hi_data=exthi(glon_min,glon_max,glat_min,glat_max,/wham,/quiet,datadir=dir)
      var_flag=-1
   endif

    line_subindex=spectnear(hi_data,glon[i],glat[i],radius,count)
	if (count eq 0) AND (num eq 1) then begin
		print,''
		print,'*** Choose a larger radius and try again! ****'
		print,''
    undefine,hi_data
		return,0;
	endif else while count eq 0 do begin
      line_subindex=spectnear(hi_data,glon[i],glat[i],radius,count)
      radius=radius+0.01
      ;whammap,hi_data,0,0,hi_data.glon,/useimage
  endwhile
  if radius ne orig_radius then print,'Increasing radius for sightline',glon[i],glat[i],' to ',radius,format='(A-32,2f-6.1,A-4,f-6.2)'

    line_submap=hi_data[line_subindex]

    ;This is only used to correct headers for the sparith function
           line_ave = REPLICATE({name:'',vel:fltarr(n_elements(hi_data[0].vel)),$
           data:fltarr(n_elements(hi_data[0].data)), $  
           var:fltarr(n_elements(hi_data[0].data)),glon:1.0,glat:1.0}, n_elements(line_submap)) 

    ;This map data doesn't have have the correct header format for the sparith function, this is to fix that
    line_ave.data=line_submap.data
    line_ave.vel=line_submap.vel
    ;Kalberla+2005, The rms brightness-temperature noise of the merged database is 0.07-0.09 K, 0.0389^2.0 before 
    if var_flag eq -1 then line_ave.var=line_submap.data*0 + 0.09^2.0 else line_ave.var=line_submap.var
    line_ave.glon=glon[i]
    line_ave.glat=glat[i]

    if keyword_set(no_ave) then return, line_ave;

    line_ave=sparith(line_ave,/ave)

    ;This iteration is to get rid of the N array
           line_ave_again = REPLICATE({name:'',vel:fltarr(n_elements(hi_data[0].vel)),$
           data:fltarr(n_elements(hi_data[0].data)), $  
           var:fltarr(n_elements(hi_data[0].data)),glon:1.0,glat:1.0}, 1) 

           line_ave_again.vel=line_ave.vel
	   line_ave_again.data=line_ave.data
	   line_ave_again.var=line_ave.var
	   line_ave_again.glon=line_ave.glon
	   line_ave_again.glat=line_ave.glat

    if i eq 0 then $
           spectra = REPLICATE({name:'',vel:fltarr(n_elements(hi_data[0].vel)),$
           data:fltarr(n_elements(hi_data[0].data)), $  
           var:fltarr(n_elements(hi_data[0].data)),glon:1.0,glat:1.0}, num) 

    spectra[i]=line_ave_again

    undefine,hi_data

endfor

if keyword_set(save) then begin
   save,filename=strcompress('hi_spectra_'+string(spectra[0].glon)+'_'+string(spectra[0].glat),/re)+'.sav',spectra
endif

if keyword_set(spe) then begin
   writespe,strcompress('hi_spectra_'+string(spectra[0].glon)+'_'+string(spectra[0].glat),/re)+'.spe',spectra.vel,spectra.data,spectra.var,n_elements(spectra.vel)
endif

  num=n_elements(line_subindex)

	return, spectra;

end