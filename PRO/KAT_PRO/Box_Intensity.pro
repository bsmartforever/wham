; pro Box_Intensity
;   Use to calculate the intensity in a user defined box on a map.
;   The program with ask the user to specify 4 limiting points and 
;   the program will calculate the emission within the boxed region.
;   The calcualted intensity is returned. 
;
;   To call: intensity=Box_intensity(map)
;
;   map = Typical structure type used to create WHAM maps (has extensions like map.vel and map.data)
;   vmin & vmax = If these keywords are not set, then the program will default to use the
;                 min and max of map(0).vel. If the specified vel are invalid, a prompt will 
;                  come up and ask you to choose values between vmin & vmax
;   ha = Keyword to specify that you want to divide the ADU intensity units by 22.8 to get R
;        To set keyword: map_show_spec, map, map_values, /ha
;   radius = Allows user to specify the radius within the map click location to include in computing
;            the averaged plotted spectra. If not specified, the default is 1 degree which is consistent
;            with the WHAM beamsize 
;
;   Note: The reason why a new map is first made in a new window is because when you leave a window and come 
;         back to it (e.g. if you run this program twice) idl seems to not remember the data displayed in that 
;         window so cursor,x,y,/data cannot be used. As I'm trying to get the glat and glon from the map, this
;         does not work well. So instead a new map is produced each time the program is ran so the data in the 
;         window is in idl's active memory. However, it is nice to have the map redisplayed in a new window so 
;         the user can play with the vmin and vmax.
;
;   Created by: Kat Barger June 2010 

function Box_Intensity,map,vmin=vmin,vmax=vmax,ha=ha,radius=radius

  ;set default keywords if user did not specify
  IF NOT keyword_set(ha) THEN factor = 1.0 else factor=22.8
  IF NOT keyword_set(vmin) THEN vmin = min(map(0).vel)
  IF NOT keyword_set(vmax) THEN vmax = max(map(0).vel)
  IF NOT keyword_set(radius) THEN radius = 1.0

;velocity error checking
try_again:
  if vmin gt vmax then begin
     temp=vmin  
     vmin=vmax
     vmax=temp
  endif

 if vmin gt max(map(0).vel) then begin
    print, "Invalid velocity range."
    print, strcompress("Choose a vmin & vmax between:"+string(min(map(0).vel))+"  "+string(max(map(0).vel))) 
    read, vmin, vmax
    goto,try_again
 endif

 if vmax lt min(map(0).vel) then begin
    print, "Invalid velocity range."
    print, strcompress("Choose a vmin & vmax between:"+string(min(map(0).vel))+"  "+string(max(map(0).vel))) 
    read, vmin, vmax
    goto,try_again
 endif

 if vmax eq vmin then begin
    print, "Invalid velocity range. vmax cannot equal vmin."
    print, strcompress("Choose a vmin & vmax between:"+string(min(map(0).vel))+"  "+string(max(map(0).vel))) 
    read, vmin, vmax
    goto,try_again
 endif

;To bring the user back to the original window when program is done
Original_Win_index=!d.window

;plot the map in a new window
window,1

;Calculate the intensity of the map values between vmin & vmax.
;Divide by 22.8 IF user uses keyword /ha to go from ADU to R
map_values=intmap(map,vmin=vmin,vmax=vmax)/factor

zmin = min(map_values) & zmax = max(map_values)

whammap, map,0,0,map_values, /useimage, $
    smgrid = 0.5,scale = 1.25,$
    title = strcompress('v!dLSR!n = '+string(vmin)+' to '+string(vmax)+' km s!u-1!n'), $
    xtitle='Galactic Longitude [Degrees]',$
    ytitle='Galactic Latitude [Degrees]',$ 
    ymargin=[10.5,5.5], xmargin=[8,8], missing = missing, _extra = extra,$
    zmin = zmin, zmax = zmax, smooth = 1

xyouts,0.38,0.05,'Galactic Longitude [Degrees]',/normal,charsize=1.2
xyouts,0.16,0.43,'Galactic Latitude [Degrees]',/normal,charsize=1.2,orientation=90


;x=glon & y=glat
print,"Box the HI emission in new window. Going Counter Clockwise."
print,"Upper Left:"
   cursor,xUL,yUL,/up
   xUL=-xUL
   print,xUL,yUL
   oplot,-[xUL],[yUL],psym=symcat(46),symsize=3,color=FSC_Color('Orchid')
print,"Upper Right:"
   cursor,xUR,yUR,/up
   xUR=-xUR
   print,xUR,yUR
   oplot,-[xUR],[yUR],psym=symcat(46),symsize=3,color=FSC_Color('Orchid')
   oplot,-[xUL,xUR],[yUL,yUR],linestyle=0,thick=2.5,color=FSC_Color('Orchid')
print,"Lower Right:"
   cursor,xLR,yLR,/up
   xLR=-xLR
   print,xLR,yLR
   oplot,-[xLR],[yLR],psym=symcat(46),symsize=3,color=FSC_Color('Orchid')
   oplot,-[xUR,xLR],[yUR,yLR],linestyle=0,thick=2.5,color=FSC_Color('Orchid')
print,"Lower Left:"
   cursor,xLL,yLL,/up
   xLL=-xLL
   print,xLL,yLL
   oplot,-[xLL],[yLL],psym=symcat(46),symsize=3,color=FSC_Color('Orchid')
   oplot,-[xLR,xLL],[yLR,yLL],linestyle=0,thick=2.5,color=FSC_Color('Orchid')
   oplot,-[xUL,xLL],[yUL,yLL],linestyle=0,thick=2.5,color=FSC_Color('Orchid')

;intercept=limit(0) & slope=limit(1)
toplimit=linfit([xUL,xUR],[yUL,yUR])
leftlimit=linfit([xUR,xLR],[yUR,yLR])
bottomlimit=linfit([xLL,xLR],[yLL,yLR])
rightlimit=linfit([xUL,xLL],[yUL,yLL])

;oplot,-glon,toplat
;oplot,-glon,leftlat
;oplot,-glon,bottomlat
;oplot,-glon,rightlat

;map=ca_data
;map_values=ca_int
map_sub=map
int_sub=map_values
glon=map.glon

toplat=toplimit(0)+toplimit(1)*glon
topindex=where(map.glat lt toplat)
map_sub=map_sub(topindex)
int_sub=int_sub(topindex)

glon=map_sub.glon
leftlat=leftlimit(0)+leftlimit(1)*glon
leftindex=where(map_sub.glat gt leftlat)
map_sub=map_sub(leftindex)
int_sub=int_sub(leftindex)

glon=map_sub.glon
bottomlat=bottomlimit(0)+bottomlimit(1)*glon
bottomindex=where(map_sub.glat gt bottomlat)
map_sub=map_sub(bottomindex)
int_sub=int_sub(bottomindex)

glon=map_sub.glon
rightlat=rightlimit(0)+rightlimit(1)*glon
rightindex=where(map_sub.glat lt rightlat)
map_sub=map_sub(rightindex)
int_sub=int_sub(rightindex)

;Plot spectra between vmin & vmax in a new window
window,3
spectrum_Win_index=3

wait,2 & erase   
multiplot,[1,4]
   xyouts,0.2,0.92,'Top Right',/normal
   subindex=spectnear(map,xUL,yUL,radius)
   ave_spec=sparith(map(subindex),/average)      
   plot,ave_spec.vel,ave_spec.data,xrange=[vmin,vmax],xstyle=1,ytitle="Intensity/(km/s)"
multiplot  
   xyouts,0.2,0.685,'Top Left',/normal                       
   subindex=spectnear(map,xUR,yUR,radius)
   ave_spec=sparith(map(subindex),/average)      
   plot,ave_spec.vel,ave_spec.data,xrange=[vmin,vmax],xstyle=1,ytitle="Intensity/(km/s)"
multiplot        
   xyouts,0.2,0.46,'Bottom Left',/normal              
   subindex=spectnear(map,xLL,yLL,radius)
   ave_spec=sparith(map(subindex),/average)      
   plot,ave_spec.vel,ave_spec.data,xrange=[vmin,vmax],xstyle=1,ytitle="Intensity/(km/s)"
multiplot               
   xyouts,0.2,0.23,'Botom Right',/normal        
   subindex=spectnear(map,xLR,yLR,radius)
   ave_spec=sparith(map(subindex),/average)      
   plot,ave_spec.vel,ave_spec.data,xrange=[vmin,vmax],xstyle=1,xtitle="Velocity [km/s]",ytitle="Intensity/(km/s)"

multiplot,/reset 
wait,2 & erase 
multiplot,[1,1],/init,/verbose 

;Make original user window active
wset,Original_Win_index

print,'The total intensity in this region is: ', total(int_sub)
return,total(int_sub)

end