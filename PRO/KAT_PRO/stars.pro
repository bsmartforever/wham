function stars, glon, glat, radius=radius, faint=faint, quiet=quiet, help=help

; Purpose - To check for bright stars along a sightline.
;
;   glon - Galactic longitude, can pass an array
;   glat - Galactic latitude, can pass an array
;   radius - Radial distance around glon and glat 
;            to search for a bright star. 
;            [DEFAULT]: radius = 0.55 - the WHAM beam size.
;   faint - The mv limit to search for bright stars. Flags when star with 
;            visual magnitudes brighter than the limit are present.  
;           [DEFAULT]: faint = 6.0 - value used in WHAM north survey.
;
;   quiet - [OPTIONAL] If quiet = 1, then skip terminal output.
;   help - [OPTIONAL] If help = 1, then remind user what the flag values mean.
;           
; Output:
;   structure containing the glon, glat, star flag (0=no star; 1=bright star), mv, 
;             and distance to the star in degrees.
;
; [CALLS] whamsao - a WHAM command line function.
;
; Examples:
;   test=stars([100.,200.],[30.,45.])
;
;   Searching for bright stars...
;   glon      glat      flag      mag       distance  
;   100.00    30.00     0         0.0       0.00      
;   200.00    45.00     0         0.0       0.00      
;  
;   help,test[0],/str
;   ** Structure <38127a8>, 5 tags, length=20, data length=20, refs=2:
;   GLON            FLOAT           100.000
;   GLAT            FLOAT           30.0000
;   FLAG            FLOAT           0.00000
;   MAG             FLOAT           0.00000
;   DIST            FLOAT           0.00000
;
;   test=stars([100.,200.],[30.,45.],faint=10)
;   Searching for bright stars...
;   glon      glat      flag      mag       distance  
;   100.00    30.00     1         7.0       0.19      
;   200.00    45.00     1         8.5       0.11  
;
;   test=stars([100.,200.],[30.,45.],faint=4.0,radius=5.0)
;   Searching for bright stars...
;   glon      glat      flag      mag       distance  
;   100.00    30.00     1         3.7       3.59      
;   200.00    45.00     0         0.0       0.00      
;
;
; By Dr. Kat Barger in March 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;The WHAM survey used 6 mag and 0.55 degrees as the cutoff
if (NOT keyword_set(faint)) then faint=6. 
if (NOT keyword_set(radius)) then star_radius=0.55 else star_radius=radius

if (NOT keyword_set(quiet)) then print,'Searching for bright stars...'
if keyword_set(help) then print,'Flag: 0=no star; 1=bright star'

num=n_elements(glon)
glon=fltarr(num)+glon
glat=fltarr(num)+glat
mag=fltarr(num)
distance=fltarr(num)

;removing stars
stars_flag=fltarr(num)

for k=0L, num-1 do begin 
    command=strcompress('whamsao -gal -rad'+ string(star_radius) +' -faint '+string(faint)+''+string(glon[k],glat[k]))
    for j=0, n_elements(command)-1 do begin
        spawn,command(j),result
        no=strsplit(result(7),/extract)
        spawn,command(j),result
        no=strsplit(result(7),/extract)
     endfor
    if no(0) ne 'No' then begin
       stars_flag(k) = 1
       junk=strsplit(result(10),/extract)
       mag[k]=junk[3]
       junk1=strsplit(result(11),"+,D,'",/extract)
       junk2=strsplit(junk1[3],'"',/extract)
       distance[k]=junk1[1]+junk1[2]/60.0+junk2/(60.0*60.0)
    endif
endfor

if (NOT keyword_set(quiet)) then begin
   print,'glon','glat','flag','mag','distance',format='(5(A-10))'
   for i=0,num-1 do print,glon[i],glat[i],stars_flag[i],mag[i],distance[i],format='(f-10.2,f-10.2,i-10,f-10.1,f-10.2)'
endif

star_str=replicate({glon:glon[0],glat:glat[0],flag:stars_flag[0],mag:mag[0],dist:distance[0]},num)
star_str.glon=glon
star_str.glat=glat
star_str.flag=stars_flag
star_str.mag=mag
star_str.dist=distance

return,star_str;


end
