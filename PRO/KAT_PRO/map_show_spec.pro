pro map_show_spec, map,vmin=vmin,vmax=vmax,ha=ha,hi=hi,radius=radius,noclick=noclick,again=again,glon=glon,glat=glat,smooth=smooth,error_map=error_map,faint=faint,star_radius=star_radius

;   This program will create a new map of your data that you are then able to interact. The program will 
;   ask you to click on a this new map and will produce a spectra of for that location. This program by
;   default will choose the pointings within a 0.6 degree radius to match WHAM with the noclicked keyword
;   option set to produce an average spectra and intensity or column density for that location.
;
;   To call: map_show_spec, Hi_map, /hi    or
;            map_show_spec, Ha_map, /ha
;            /hi or /ha MUST be specified
;
;   map = Typical structure type used to create WHAM maps (has extensions like map.vel and map.data)
;   vmin & vmax = If these keywords are not set, then the program will default to use the
;                 min and max of map(0).vel. If the specified vel are invalid, a prompt will 
;                 ask you to choose values between vmin & vmax
;   ha = Keyword to specify that you want to divide the ADU intensity units by 22.8 to get R
;        To set keyword: map_show_spec, map, map_values, /ha
;   radius = Allows user to specify the radius within the map noclick location to include in computing
;            the averaged plotted spectra. If not specified, the default is 1 degree which is consistent
;            with the WHAM beamsize 
;   noclick = Allows the user to manually enter glon and glat instead of clicking on the map.
;   again = Allows user reselect locations to produce a spectra plot. If /again is specified, the user 
;           is able to select locations by left clicking on the map and exit the program by right clicking.
;           If both /again & /no click are specified, then the user may exit the program by typing in 'q' 
;           for both the glon and glat. 
;   error_map = Show the an error map of the Ha instead of the intensity. When you click on the map, the spectrum
;               and intensity for that locations will still be calculated, but this allows for the search of 
;               background stars. Nearby stars with < 9 mag will be listed. (Note: most useful when smooth = 0)
;   Faint = Allows the user to specify the cut off brightness of nearby stars (default = < 9 mag) 
;   star_radius =  Allows the user to specify the radius to search for stars (default = 0.6 degrees)
; 
;
;   Note: The reason why a new map is first made in a new window is because when you leave a window and come 
;         back to it (e.g. if you run this program twice) idl seems to not remember the data displayed in that 
;         window so cursor,x,y,/data cannot be used. As I'm trying to get the glat and glon from the map, this
;         does not work well. So instead a new map is produced each time the program is ran so the data in the 
;         window is in idl's active memory. However, it is nice to have the map redisplayed in a new window so 
;         the user can play with the vmin and vmax.
;
;   e.g.  map_show_spec,ca_data,/ha,vmin=-220,vmax=-110,/noclick,/again  
;
;   Created by: Kat Barger July 2010 

;To bring the user back to the original window when program is done
Original_Win_index=!d.window

  ;set default keywords if user did not specify
  IF keyword_set(ha) THEN factor=1.0/22.8
  IF keyword_set(hi) THEN factor=1.8224e18
     error_factor=1.0/22.8^2.0
  IF NOT keyword_set(faint) THEN faint=9.0
  IF NOT keyword_set(star_radius) THEN star_radius=0.6
  IF keyword_set(smooth) THEN smooth=1
  IF ((NOT keyword_set(ha)) and (NOT keyword_set(hi))) THEN begin
     print,''
     print,"***********************************************"
     print,"*** Please try again and specify /hi or /ha ***" 
     print,"***********************************************"
     print,''
     goto,end_of_program
  ENDIF ELSE begin
     hi_ha=where(tag_names(map) eq 'VAR',num_hi_ha)
     IF (keyword_set(hi)) and (num_hi_ha ne 0) THEN begin
        print,''
        print,"*****************************************************"
        print,"*** This is H-alpha data, changing to /ha keyword ***"
        print,"*****************************************************"
        print,''
        ha=1
        hi=0
     ENDIF
     IF (keyword_set(ha)) and (num_hi_ha ne 1) THEN begin
        print,''
        print,"************************************************"
        print,"*** This is HI data, changing to /hi keyword ***"
        print,"************************************************"
        print,''
        ha=0
        hi=1
     ENDIF
  ENDELSE
  IF (NOT keyword_set(vmin)) and (NOT keyword_set(vmax)) THEN begin
     vmin = min(map(0).vel)
     vmax = max(map(0).vel)
  ENDIF
  IF NOT keyword_set(radius) THEN radius = 0.6
  IF keyword_set(again) and (NOT keyword_set(noclick)) then begin
     print,''
     print,"***********************************"
     print,"*** Right noclick to End Program ***" 
     print,"**********************************"
     print,''
  ENDIF
  IF keyword_set(noclick) and keyword_set(again) THEN begin
     print,''
     print,"************************************************"
     print,"*** Type Q for glon and glat to end program. ***" 
     print,"************************************************"
     print,''
  endif
  IF (keyword_set(glon)) and (keyword_set(glat)) then glon_glat=1 else glon_glat=0


  If Not keyword_set(noclick) THEN noclick=0.0 else noclick=1.0

!p.symsize=2.5
!p.charthick=2.0
!p.charsize=1.25
!p.thick=1.5

@mycmap

;velocity error checking
try_again:
  if vmin gt vmax then begin
     temp=vmin  
     vmin=vmax
     vmax=temp
  endif

  if vmin lt min(map(0).vel) then begin
     print,"Specified vmin less then minimum velocity, changing to the minimum: ",vmin,format='(A-69,f6.1)'  
     vmin=min(map(0).vel)
  endif

  if vmax gt max(map(0).vel) then begin
     print,"Specified max less then maximum velocity, changing to the maximum: ",vmax,format='(A-67,f6.1)' 
     vmax=max(map(0).vel)
  endif

 if vmin gt max(map(0).vel) then begin
    print, "Invalid velocity range."
    print, strcompress("Choose a vmin & vmax between:"+string(min(map(0).vel))+"  "+string(max(map(0).vel))) 
    vmin=''
    vmax='' 
    read,"vmin: ", vmin
    read,"vmax: ", vmax
    test_num_vmin=validate_numeric(vmin)
    test_num_vmax=validate_numeric(vmax)
 
    while ((test_num_vmin eq 0) and (test_num_vmax eq 0)) do begin
       print,'Invalid number, try again.'
       read,"vmin: ", vmin
       read,"vmax: ", vmax
       test_num_vmin=validate_numeric(vmin)
       test_num_vmax=validate_numeric(vmax)
    endwhile
    vmin=fix(vmin,type=4)
    vmax=fix(vmax,type=4)
    goto,try_again
 endif

 if vmax lt min(map(0).vel) then begin
    print, "Invalid velocity range."
    print, strcompress("Choose a vmin & vmax between:"+string(min(map(0).vel))+"  "+string(max(map(0).vel))) 
    vmin=''
    vmax='' 
    read,"vmin: ", vmin
    read,"vmax: ", vmax
    test_num_vmin=validate_numeric(vmin)
    test_num_vmax=validate_numeric(vmax)
 
    while ((test_num_vmin eq 0) and (test_num_vmax eq 0)) do begin
       print,'Invalid number, try again.'
       read,"vmin: ", vmin
       read,"vmax: ", vmax
       test_num_vmin=validate_numeric(vmin)
       test_num_vmax=validate_numeric(vmax)
    endwhile
    vmin=fix(vmin,type=4)
    vmax=fix(vmax,type=4)
    goto,try_again
 endif

 if vmax eq vmin then begin
    print, "Invalid velocity range. vmax cannot equal vmin."
    print, strcompress("Choose a vmin & vmax between:"+string(min(map(0).vel))+"  "+string(max(map(0).vel))) 
    vmin=''
    vmax='' 
    read,"vmin: ", vmin
    read,"vmax: ", vmax
    test_num_vmin=validate_numeric(vmin)
    test_num_vmax=validate_numeric(vmax)
 
    while ((test_num_vmin eq 0) and (test_num_vmax eq 0)) do begin
       print,'Invalid number, try again.'
       read,"vmin: ", vmin
       read,"vmax: ", vmax
       test_num_vmin=validate_numeric(vmin)
       test_num_vmax=validate_numeric(vmax)
    endwhile
    vmin=fix(vmin,type=4)
    vmax=fix(vmax,type=4)
    goto,try_again
 endif

;Calculate the intensity of the map values between vmin & vmax.
;Divide by 22.8 IF user uses keyword /ha to go from ADU to R
;Multipy by 1.8224e18 IF user uses keyword /hi to get column density
  IF keyword_set(ha) THEN map_values=intmap(map,vmin=vmin,vmax=vmax)*factor
  IF keyword_set(error_map) THEN map_values=interrmap(map,vmin=vmin,vmax=vmax)*error_factor
  IF keyword_set(hi) THEN map_values=intmap(map,vmin=vmin,vmax=vmax,/hi)*factor

   !MOUSE.button = 1
   plot_location=0
   pos_lon_old=(max(map.glon)-min(map.glon))/2.0+min(map.glon)
   pos_lat_old=(max(map.glat)-min(map.glat))/2.0+min(map.glat)
   WHILE (!MOUSE.button NE 4) DO BEGIN  

;plot the map in a new window
window,1
zmin = 0 & zmax = max(map_values)
if zmin lt 0 then zmin=0

whammap, map,0,0,map_values, /useimage, $
    smgrid = 0.5,scale = 1.25,$
    title = strcompress('v!dLSR!n = '+string(vmin)+' to '+string(vmax)+' km s!u-1!n'), $
    xtitle='Galactic Longitude [Degrees]',$
    ytitle='Galactic Latitude [Degrees]',$ 
    ymargin=[10.5,5.5], xmargin=[8,8], missing = missing, _extra = extra,$
    zmin = zmin, zmax = zmax, smooth = smooth,beamrad=0.25

xyouts,0.38,0.05,'Galactic Longitude [Degrees]',/normal,charsize=1.2
xyouts,0.16,0.43,'Galactic Latitude [Degrees]',/normal,charsize=1.2,orientation=90

if plot_location eq 1 then plots,-pos_lon,pos_lat,psym=symcat(46),color=FSC_Color('Orchid')

 noclick_again:
      if (noclick eq 0) then begin 
          If glon_glat eq 0 then begin
             print,"Click on the map to plot the spectra at that location"
             cursor,pos_lon_new,pos_lat_new,/up
          endif else if (glon_glat eq 1) then begin
             pos_lon_new=-glon
             pos_lat_new=glat
          endif
          if (!mouse.button ne 4) or (NOT keyword_set(pos_lat)) or (NOT keyword_set(pos_lon)) then begin
             pos_lon=pos_lon_new
             pos_lat=pos_lat_new
          endif
      endif else begin 
            pos_lon=''
            pos_lat=''
         print,''
         print,"Enter glat and glon locations on the map."
         read,"gLon: ",pos_lon 
         read,"gLat: ",pos_lat 
         if (pos_lon eq 'q') or (pos_lon eq 'Q') or (pos_lat eq 'q') or (pos_lat eq 'Q') then begin
             pos_lon=pos_lon_old
             pos_lat=pos_lat_old
             print,strcompress("glon: "+string(pos_lon,format='(f8.1)')+"  glat: "+string(pos_lat,format='(f6.1)'))
             !mouse.button=4
         endif
      endelse
         test_num_pos_lon=validate_numeric(pos_lon)
         test_num_pos_lat=validate_numeric(pos_lat)
         while ((test_num_pos_lon eq 0) or (test_num_pos_lat eq 0)) do begin
            print,'Invalid number, try again.'
            pos_lon=''
            pos_lat=''
            read,"gLon: ", pos_lon
            read,"gLat: ", pos_lat
            test_num_pos_lon=validate_numeric(pos_lon)
            test_num_pos_lat=validate_numeric(pos_lat)
         endwhile
      pos_lon=fix(pos_lon,type=4) & pos_lat=fix(pos_lat,type=4)
      if (noclick eq 0) and (!mouse.button ne 4) then pos_lon=-pos_lon
      if ((pos_lon lt max(map.glon)) and (pos_lon gt min(map.glon)) and (pos_lat lt max(map.glat)) and (pos_lat gt min(map.glat)) and (!mouse.button ne 4)) then begin
         pos_lon_old=pos_lon
         pos_lat_old=pos_lat
      endif
      if ((pos_lon gt max(map.glon)) or (pos_lon lt min(map.glon)) or (pos_lat gt max(map.glat)) or (pos_lat lt min(map.glat)) and (!mouse.button ne 4)) then begin
         print,''
         print,"*******************************************"
         print,"*** Please select a location the map! ***" 
         print,"*** Reseting to last valid position   ***" 
         print,"*******************************************"
         pos_lon=pos_lon_old
         pos_lat=pos_lat_old
         print,strcompress("glon: "+string(pos_lon,format='(f8.1)')+"  glat: "+string(pos_lat,format='(f6.1)'))
         goto,noclick_again
      endif

plots,-pos_lon,pos_lat,psym=symcat(46),color=FSC_Color('Orchid')
plot_location=1

print,''
print,strcompress("glon: "+string(pos_lon,format='(f8.1)')+"  glat: "+string(pos_lat,format='(f6.1)'))
subindex=spectnear(map,pos_lon,pos_lat,radius)

           ave_spec_template = REPLICATE({name:"Ave_Spec", vel:fltarr(n_elements(map(0).data)),$
           data:fltarr(n_elements(map(0).data)), $  
           var:fltarr(n_elements(map(0).data))}, n_elements(subindex)) 

           ave_spec_template.data=map(subindex).data 
           ave_spec_template.vel=map(subindex).vel
           ave_spec_template.var=map(subindex).data

   ave_spec=sparith(ave_spec_template(subindex),/average) 

;Plot spectra between vmin & vmax in a new window
window,3
spectrum_Win_index=3



plot,ave_spec.vel,ave_spec.data,xtitle="Velocity [km/s]",ytitle="Intensity/(km/s)",$
title="glon: "+string(pos_lon,format='(f6.1)')+"  glat: "+string(pos_lat,format='(f6.1)'),$
xrange=[vmin,vmax],xstyle=1,charsize=1.5,charthick=1

If keyword_set(ha) then print,"Intensity:", strcompress(string(int_tabulated(ave_spec.vel(where((ave_spec.vel ge vmin) and (ave_spec.vel le vmax))),ave_spec.data(where((ave_spec.vel ge vmin) and (ave_spec.vel le vmax))))*factor,format='(e-25.2)')),'+/-',strcompress(string(interrmap(ave_spec,vmin=vmin,vmax=vmax)*factor,format='(e-25.2)')),format='(A-15,A-10,A-5,A-10)'

;Intmap.pro doesn't seem to work well when it is not given a map so I'm using int_tabulated instead
If keyword_set(hi) then print,"Column Density:", strcompress(string(int_tabulated(ave_spec.vel(where((ave_spec.vel ge vmin) and (ave_spec.vel le vmax))),ave_spec.data(where((ave_spec.vel ge vmin) and (ave_spec.vel le vmax))))*factor,format='(e-25.2)')),'+/-',strcompress(string(interrmap(ave_spec,vmin=vmin,vmax=vmax)*factor,format='(e-25.2)')),format='(A-17,A-10,A-5,A-10)'

print,factor

   IF NOT keyword_set(again) THEN !MOUSE.button = 4
   IF keyword_set(error_map) THEN begin
      command=strcompress('whamsao -rad'+ string(star_radius) +' -gal '+' -faint '+string(faint)+''+string(pos_lon,pos_lat))
      print,command
      ;This calls the c program whamsao from the terminal
      spawn,command
   ENDIF
   ENDWHILE  

end_of_program:
;Make original user window active
wset,Original_Win_index

end