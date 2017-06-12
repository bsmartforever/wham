pro off_loc, help=help, magellanic=magellanic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Purpose: Determine location of off position by clicking on the 
;          already plotted map. 
;
; magellanic - if HI map is in Magellanic coordinants, use this keyword to 
;              output location in Galactic coordinants
;
; Outputs: -The glon and glat of clicked position.
;          -Warning on whether or not bright stars are clicked position
;           with criteria being faint stars of Mv=6 and brighter within
;           0.55 degrees.
;
; Notes: Run find_off.pro first
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(help) then begin
   print,''
   print,'Determines the off position by outputting the map location the user clicks on.'
   print,'run find_off.pro first.'
   print,''  
   return;
endif

print,'Click on desired off location.'
cursor,glon,glat
glon=-glon

;The WHAM survey used 6 mag and 0.55 degrees as the cutoff
faint=6 
star_radius=0.55

    command=strcompress('whamsao -gal -rad'+ string(star_radius) +' -faint '+string(faint)+''+string(glon,glat))
    for j=0, n_elements(command)-1 do begin
        spawn,command(j),result
        no=strsplit(result(7),/extract) 
     endfor
    if no(0) eq 'No' then print,'No bright stars.'
    if no(0) ne 'No' then print,'*** Bright stars, choose a different off. ***'

if glon lt 0 then glon=360.0-abs(glon)
if keyword_set(magellanic) then begin
   print,'Off Mag location: ',glon,glat,format='(A-20,f-8.1,f-8.1)'
   mag2gal,glon,glat,l,b
   glon=l
   glat=b
   print,'Off Gal location: ',glon,glat,format='(A-20,f-8.1,f-8.1)'
endif else $
print,'Off location: ',glon,glat,format='(A-15,f-8.1,f-8.1)'

end