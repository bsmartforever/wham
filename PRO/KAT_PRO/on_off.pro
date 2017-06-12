function on_off, on, off, scale_velocity=scale_velocity, line=line, bin=bin

; Purpose - Calculate Ons - Offs grouped closest in time. 
;
; [optional]
; scale_velocity - Use pressures to calculate velocity.
; line - a string containing the filter name of form:
;        'ha','ha_omega','sii','nii','hb','oi','oii','oiii','nii_blue', or 'hei'
; bin - Change the array size of vel, data, var. 
;       Default binning is to rescale arrays to half their size.
;       For bin > 1, the array will be rescale to that value.  
;
; NOTE: Need to .compile scale_velocity before running

!except=0 ;To remove annoying % Program caused arithmetic error: Floating divide by 0 message
on_temp=on
off_temp=off

On_Off=replicate({name:' ',vel:On[0].vel,data:On[0].data,var:On[0].var,$
       glon:On[0].glon,glat:On[0].glat},n_elements(On))

if Keyword_set(scale_velocity) then begin
   if (NOT Keyword_set(line)) then begin
       substrings = strsplit(on[0].name,'/',/extract)
       lines=['ha','ha_omega','ha_red','ha_blue','sii','nii','hb','oi','oii','oiii','nii_blue','hei']
       count=0
       i=0
       while(count eq 0) do begin
           junk=where(substrings eq lines[i],count)
           if count ne 0 then line=substrings[junk[0]]
           if (i eq n_elements(lines)-1) and (count eq 0) then begin
              print,'*** No lines indicated in *.name, please supply ***'
              return,0;
           end
           i=i+1
       endwhile
   endif
   if (line eq 'ha_omega') or (line eq 'ha_blue') or (line eq 'ha_red') then line='ha'
   for j=0, n_elements(on)-1 do $
       on_temp[j].vel=scale_velocity(obs=On[j],line=line,/silent)
   for j=0, n_elements(off)-1 do $
       off_temp[j].vel=scale_velocity(obs=off[j],line=line,/silent)
endif

on_ave=sparith(on,/ave)
off_ave=sparith(off,/ave)
On_Off=sparith(on_ave,off_ave,/sub,/samenum)
;Matching in time shouldn't actually matter
;for i=0, n_elements(On_Off)-1 do begin
;    junk=min(abs(On[i].time-Off.time),min_time_off)
;    on_match=On_temp[i]
;    off_match=Off_temp[min_time_off]
;    On_Off[i]=sparith(on_match,off_match,/sub,/samenum)
;endfor

if Keyword_set(bin) then begin
    if bin eq 1 then $
       On_Off=rebin_spectra(On_Off,n_elements(on[0].vel)/2.0) $
    else $
       On_Off=rebin_spectra(On_Off,bin)
endif

stars_on=stars(on_ave.glon,on_ave.glat,/quiet)
stars_flag_on=stars_on.flag
stars_off=stars(off_ave.glon,off_ave.glat,/quiet)
stars_flag_off=stars_off.flag

if stars_flag_on ne 0 then $
   print,'*** Stars near On! ***'
if stars_flag_off ne 0 then $
  print,'*** Stars near OFF! ***'

    if stars_flag_off eq 1 then On_Off_ave.name='stars near off '+On_Off.name
    if stars_flag_on eq 1 then On_Off_ave.name='stars near on '+On_Off.name
    return, on_off;

end 