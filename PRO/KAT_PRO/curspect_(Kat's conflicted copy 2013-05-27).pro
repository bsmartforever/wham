PRO curspect, map, map2, map3, map4, map_scale = map_scale, $
              intrange = intrange, image = image, radec = radec, $
              ;hi1 = hi1, hi2 = hi2, $
              radius = radius, arr_color=arr_color,$
              im_window = im_window, sp_window = sp_window,$
              magellanic=magellanic, index=index, nsum=nsum,$
              lon=lon, lat=lat, glon=glon, glat=glat, mlon=mlon, mlat=mlat,$
              device=device, ps=ps, filename=filename, $
              landscape=landscape, portrait=portrait, $
              charsize=charsize,charthick=charthick,$
              color=color,xrange=xrange,yrange=yrange,_extra=extra

;+
;NAME: curspect
;SYNTAX: curspect, map [,map2] [,map_scale=map_scale] [,intrange=intrange] $
;   [,image=image] [,/radec] [,/hi1] [,/hi2] [radius=radius]
;
;INPUTS:
;   map - map to click on
;OPTIONAL INPUTS:
;   map2 - Map to show spectra of when directions are clicked
;   map3 - Same as map2. Allows the comparison of a third spectrum.
;   map4 - Same as map2. Allows the comparison of a forth spectrum.
;   map_scale - An array containing the a data scaling qualtity for each map.
;          Accepts up to 4 values. Default scaling is 1.0.
;          e.g., map_scale = [1.0, 1./22.8]
;   nsum - Integer value that defines a quick and dirty bin factor for 
;          the spectrum. Default is 1.0 = no binning.
;          e.g., nsum = 2.0 goes from 2 km/s to 4 km/s bins. 
;   radius - Angular radius that defines the circular region that 
;          is binned together to produce the spectra. This radius
;          sets the region for all the plotted spectra from all four maps. 
;   arr_color - Defines the plotting colors for each spectra. Valid inputs
;          are strings or string arrays that contain the color name, e.g., 'blue'
;          Default colors are black, red, orchid, and blue.
;   index - Map index for all passed maps. This will cause the spectra 
;          of the given index element to be plotted instead of searching
;          for the spectra near a given longitude and latitude. 
;          This is only really useful if the elements of all the maps
;          align in coordinate space.
;   magellanic - Search for and find spectra that corresponds to 
;          magellanic longitude and latitude as opposed to galactic 
;          coordinants. The map must contain mlat and mlon tags.
;   lon, lat, glon, glat - Galactic longitude and latitude.
;          If passed, the cursor map clicking prompt is skipped and 
;          the spectra is immediately plotted. 
;   mlon, mlat - Magellanic latitude and longitude.
;          If passed, the cursor map clicking prompt is skipped and 
;          the spectra is immediately plotted. 
;   device - 'PS' or 'X' => Set plot to terminal window or to postscript.
;   ps     - Set plot output to postscript. 
;   filename - filename of output postscript. Default is curspect_{lon}_{lat}.eps
;   landscape - Orientation of output postscript. This is default.
;   portrait - Orientation of output postscript.
;- 


; 2005-8-15 ASH added radius keyword (specify radius within which to search)

p_str=!p

  if n_elements(map_scale) eq 0 then begin
    map_scale = [1.]
    if keyword_set(map2) then map_scale = [1.,1.]
    if keyword_set(map3) then map_scale = [1.,1.,1.]
    if keyword_set(map4) then map_scale = [1.,1.,1.,1.]
  end
  if keyword_set(map_scale) then begin
    num_map=1
    if keyword_set(map2) then num_map=2 
    if keyword_set(map3) then num_map=3 
    if keyword_set(map4) then num_map=4
    if num_map ne n_elements(map_scale) then begin
       map_scale=intarr(num_map)+1.
       print,'******** map_scale ne number of maps ***********' 
       print,'****** map_scale set to 1 for all maps *********' 
    endif
  endif

if size(color,/type) eq 7 then begin
   TVLCT,cgColor(color,/triple),253
   color=cgColor(color)
endif else TVLCT, 0, 0, 0, 253       ; Black color
   !p.color=253
   color=253

  if (NOT keyword_set(nsum)) then nsum=1.0
  if NOT keyword_set(arr_color) then begin
     arr_color=['black','red','orchid','blue']
  endif else arr_color=strarr(n_elements(arr_color))+arr_color

if (keyword_set(lon) AND keyword_set(lat)) or (keyword_set(glon) AND keyword_set(glat)) or (keyword_set(mlon) AND keyword_set(mlat)) then begin
   if (keyword_set(lon) AND keyword_set(lat)) then user_lat=lat
   if (keyword_set(lon) AND keyword_set(lat)) then user_lon=lon
   if (keyword_set(glon) AND keyword_set(glat)) then user_lat=glat
   if (keyword_set(glon) AND keyword_set(glat)) then user_lon=glon
   if (keyword_set(mlon) AND keyword_set(mlat)) then user_lat=mlat
   if (keyword_set(mlon) AND keyword_set(mlat)) then begin
      user_lon=mlon
      index=where(user_lon lt 0,count)
      if count ne 0 then user_lon[index]=user_lon[index]+360.
   endif
   if (keyword_set(mlon) AND keyword_set(mlat)) then magellanic=1
endif
if keyword_set(index) then ncnt=1.0


  hi1=1
  hi2=1
  hi3=1
  hi4=1  

  rad1 = 0.25
  rad2 = 0.25
  rad1 = 0.25
  rad2 = 0.25

  if n_elements(im_window) eq 0 then im_window = !d.window else im_window = im_window[0]
  if n_elements(sp_window) eq 0 then sp_window = 9
  
  IF keyword_set(radius) THEN BEGIN
	  rad1 = radius
	  rad2 = radius
	  rad3 = radius
	  rad4 = radius
  ENDIF

if keyword_set(filename) OR keyword_set(PS) then device='PS' else device='X'

  IF n_elements(image) EQ 0 AND (device ne 'PS') THEN BEGIN 
    wset_or_create, sp_window
    wset, im_window
    wshow, im_window, 1
    xs0 = !x
    ys0 = !y
  ENDIF 

  if ((NOT keyword_set(user_lon)) AND (NOT keyword_set(user_lat))) AND (NOT keyword_set(index)) then print, 'Left button plots, Middle button overplots, Right button exits'

  done = 0

  WHILE NOT done DO BEGIN 

    if ((NOT keyword_set(user_lon)) AND (NOT keyword_set(user_lat))) AND (NOT keyword_set(index)) then cursor, lon, lat, /down $
    else if ((keyword_set(user_lon)) AND (keyword_set(user_lat))) then begin
      lon=-user_lon
      lat=user_lat
    endif

    if (keyword_set(user_lon)) AND (keyword_set(user_lat)) OR (keyword_set(index)) then done = 0 $
    else done = (!mouse.button AND 4) EQ 4
    over = (!mouse.button AND 2) EQ 2

    IF NOT done THEN BEGIN
      if (NOT keyword_set(index)) then IF (lon LE 0) THEN lon = -lon ELSE lon = 360-lon
      IF keyword_set(radec) THEN $
        euler, lon, lat, lon, lat, 1
      
      if (NOT keyword_set(magellanic)) AND (NOT keyword_set(index)) then near = spectnear(map, lon, lat, rad1, ncnt) $
         else if (NOT keyword_set(index)) AND (keyword_set(magellanic)) then near = spectnear(map, lon, lat, rad1, ncnt,/magellanic)
print,lon,lat

      if keyword_set(index) then near =  index
         
       if ncnt ne 0 then begin
          lon = map[near[0]].glon
          lat = map[near[0]].glat
          if keyword_set(magellanic) then begin
             lon = map[near[0]].mlon
             lat = map[near[0]].mlat
          endif
       endif

if (device eq 'ps') or (device eq 'PS') then begin

   set_plot,device 
   if keyword_set(filename) then $
      name=validate_extension(filename,'eps') $
   else if (keyword_set(lat)) AND (keyword_set(lon)) then begin
        filename='curspect_'+strcompress(string(-lon,format='(f8.2)'),/re)+$
        '_'+strcompress(string(lat,format='(f8.2)'),/re) 
        name=validate_extension(filename,'eps')
    endif else $
   name='curspect.eps'
   entry_device=!d.name
   !p.font=0
   device,filename=name,bits_per_pixel=8,color=1
   if keyword_set(portrait) then $
      device,/portrait,ysize=10,yoffset=(11.0-10)*0.5,/inches $
   else device,/landscape,xsize=10,xoffset=(11.0-10)*0.5,/inches 
   device,encapsulated=0,/helvetica

   !x.thick=5
   !y.thick=5
   if (!p.thick le 2) AND (NOT keyword_set(thick)) then !p.thick=5

   print,'Printing to file: ',name

endif else begin

   set_plot,device
   if (!p.thick ge 4) AND (NOT keyword_set(thick)) then !p.thick=2
   if (!x.thick ge 4) AND (NOT keyword_set(thick)) then !x.thick=2
   if (!y.thick ge 4) AND (NOT keyword_set(thick)) then !y.thick=2

endelse

      ;; mapslice returns 0, a scalar if no spectra are near
      IF ncnt EQ 0 THEN BEGIN
        print, 'No spectra within ' + strtrim(rad1, 2) + ' degrees of click'
      ENDIF ELSE BEGIN 
        
        print, ncnt, ' pointing(s) near, using element ' $
          + strtrim(near[0], 2) 
        
        IF NOT keyword_set(hi1) THEN BEGIN 
          bpos = rstrpos(((map[near[0]])).name, 'b')
          shname = strmid(((map[near[0]])).name, bpos + 1)
          dotpos = rstrpos(shname, '.')          

          IF dotpos NE -1 THEN $
            shname = strmid(shname, 0, dotpos)
        ENDIF ELSE BEGIN 
          shname = 'Element ' + strtrim(near[0], 2) 
        ENDELSE 
        
        IF n_elements(image) NE 0 THEN BEGIN 
          print,  'Image value for ' + shname + ' = ' + $
            strtrim(image[near[0]], 2)
        ENDIF ELSE BEGIN 
          IF n_elements(intrange) EQ 2 THEN $
            print, 'Intensity (' + strtrim(intrange[0], 2) + ' - ' + $
            strtrim(intrange[1], 2) + ') = ' + $
            strtrim(intspect((map[near[0]]), intrange[0], $
                             intrange[1])/684.1, 2) + $
            ' R'
          
          if device ne 'PS' then wset, sp_window
          IF over THEN BEGIN
            !x = xs1
            !y = ys1
            IF NOT keyword_set(hi1) THEN begin 
              vpmin = max((map[near[0]]).vel) - 203
              velslice = where((map[near[0]]).vel GE vpmin, vcount)
              
              oplot, ((map[near[0]]).vel)[velslice], ((map[near[0]]).data)[velslice], nsum = nsum
            endif ELSE begin
              oplot, (map[near[0]]).vel, (map[near[0]]).data, nsum = nsum
            endelse
          ENDIF ELSE BEGIN 
          if (NOT keyword_set(magellanic)) then $
            title = 'Block ' + shname $
              + ' (l=' + strtrim(string(((map[near[0]])).glon,format='(f8.2)'), 2) $
              + ' b=' + strtrim(string(((map[near[0]])).glat,format='(f8.2)'), 2) + ')' $
          else $
            title = 'Block ' + shname $
              + ' (mlon=' + strtrim(string(((map[near[0]])).mlon,format='(f8.2)'), 2) $
              + ' mlat=' + strtrim(string(((map[near[0]])).mlat,format='(f8.2)'), 2) + ')'

            IF NOT keyword_set(hi1) THEN begin 
              vpmin = max(((map[near[0]])).vel) - 203
              velslice = where(((map[near[0]])).vel GE vpmin, vcount)

              plot, (((map[near[0]])).vel)[velslice], (((map[near[0]])).data)[velslice]*map_scale[0], $
                    title = title, xtitle='Velocity [km/s]',ytitle='Intensity', $
                    xstyle = 3, ystyle = 3, $
                    _extra = e, nsum = nsum,xrange=xrange,yrange=yrange
             oplot,!x.crange,[0,0],linestyle=1
            endif  ELSE begin
              plot, ((map[near[0]])).vel, ((map[near[0]])).data*map_scale[0], $
                    title = title, xtitle='Velocity [km/s]',ytitle='Intensity',$
                    xstyle = 3, ystyle = 3, $
                    _extra = e, nsum = nsum,color=color,xrange=xrange
              oplot, ((map[near[0]])).vel, ((map[near[0]])).data*map_scale[0], color=cgcolor(arr_color[0])     
             oplot,!x.crange,[0,0],linestyle=1,color=color
            endelse
             
            xs1 = !x
            ys1 = !y
          ENDELSE 

          IF n_elements(map2) NE 0 THEN BEGIN
            if (NOT keyword_set(index)) then near = spectnear(map2, lon, lat, rad2, ncnt)
            
            ;; mapslice returns 0, a scalar if no spectra are near
            IF ncnt EQ 0 THEN BEGIN
              print, 'No spectra from map #2 within ' + strtrim(rad2, 2) $
                     + ' degrees of click'
            ENDIF ELSE BEGIN 
              print, ncnt, ' pointing(s) in map #2 near, using element ' $
                + strtrim(near[0], 2) 

              IF NOT keyword_set(hi2) THEN begin 
                vpmin = max(map2[near[0]].vel) - 203
                velslice = where(map2[near[0]].vel GE vpmin, vcount)
              
                oplot, map2[near[0]].vel[velslice], $
                  map2[near[0]].data[velslice]*map_scale[1], $
                  color = cgcolor(arr_color[1]), nsum = nsum
              endif else begin 
                oplot, map2[near[0]].vel, map2[near[0]].data*map_scale[1], $
                  color = cgcolor(arr_color[1]), nsum = nsum
              ENDELSE 
            endelse 
          endif 

          IF n_elements(map3) NE 0 THEN BEGIN
            near = spectnear(map3, lon, lat, rad2, ncnt)
            
            ;; mapslice returns 0, a scalar if no spectra are near
            IF ncnt EQ 0 THEN BEGIN
              print, 'No spectra from map #2 within ' + strtrim(rad2, 2) $
                     + ' degrees of click'
            ENDIF ELSE BEGIN 
              print, ncnt, ' pointing(s) in map #2 near, using element ' $
                + strtrim(near[0], 2) 

              IF NOT keyword_set(hi2) THEN begin 
                vpmin = max(map3[near[0]].vel) - 203
                velslice = where(map3[near[0]].vel GE vpmin, vcount)
              
                oplot, map3[near[0]].vel[velslice], $
                  map3[near[0]].data[velslice]*map_scale[2], $
                  color = cgcolor(arr_color[2]), nsum = nsum
              endif else begin 
                oplot, map3[near[0]].vel, map3[near[0]].data*map_scale[2], $
                  color = cgcolor(arr_color[2]), nsum = nsum
              ENDELSE 
            endelse 
          endif 

          IF n_elements(map4) NE 0 THEN BEGIN
            near = spectnear(map4, lon, lat, rad2, ncnt)
            
            ;; mapslice returns 0, a scalar if no spectra are near
            IF ncnt EQ 0 THEN BEGIN
              print, 'No spectra from map #2 within ' + strtrim(rad2, 2) $
                     + ' degrees of click'
            ENDIF ELSE BEGIN 
              print, ncnt, ' pointing(s) in map #2 near, using element ' $
                + strtrim(near[0], 2) 

              IF NOT keyword_set(hi2) THEN begin 
                vpmin = max(map4[near[0]].vel) - 203
                velslice = where(map4[near[0]].vel GE vpmin, vcount)
              
                oplot, map4[near[0]].vel[velslice], $
                  map4[near[0]].data[velslice]*map_scale[3], $
                  color = cgcolor(arr_color[3]), nsum = nsum
              endif else begin 
                  oplot, map4[near[0]].vel, map4[near[0]].data*map_scale[3], $
                  color = cgcolor(arr_color[3]), nsum = nsum
              ENDELSE 
            endelse 
          endif 

          if device ne 'PS' then begin
             wset, im_window

             !x = xs0
             !y = ys0
          endif

        ENDELSE 
      ENDELSE 
    ENDIF 
    if (keyword_set(user_lon)) AND (keyword_set(user_lat)) OR (keyword_set(index)) then done = 1 
    if keyword_set(device) then begin
       if (device eq 'ps') or (device eq 'PS') then begin

           device,/close_file
           set_plot,entry_device
            Close, /All
    
       endif

       set_plot,'x'

    endif   
  ENDWHILE

  !p=p_str

End