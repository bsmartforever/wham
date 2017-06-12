pro find_off, vcen=vcen, width=width, loc=loc, max_HI=max_HI, blocks=blocks,$
    large_blocks=large_blocks,vwidth=vwidth,click=click,smooth=smooth,help=help,$
    ps=ps,stars=stars,magellanic=magellanic

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;+
; Purpose: Plots the region of interest in HI (LAB survey), labeling 
;          the on location with a diamond and locations near bight 
;          stars with plus symbols. 
;
; vcen - central velocity of the emission. Default is 0.
; width - width in degrees to define max and min of glon and glat of
;         the plotted region
; max_HI - maximum HI value expected. Input to whammap. Default is 
;          5e18 cm^-2
; blocks - overlap the positions of the survey blocks, labeling block numbers 
;          and pointing locations with a '+' of blocks # between 1–1234
; large_blocks - overlap the positions of the non-survey blocks, labeling block numbers 
;          and pointing locations with a '+' of blocks # of 1234+
; vwidth - set the integration velocity width, centered at vcen. 
;	   Useful when making channel maps or when avoiding Galactic emission.
; loc - [glon,glat] of the on position
; click - call curvspec, allowing user to preview the spectra
; smooth - The smooth value for whammap, where 0 = beams [DEFAULT],
;          1=smooth, 2=smooth with clean and extrapolated edges.  
; magellanic - plot HI map in Magellanic coordinants. loc is still expected
;          to be in Galactic coordinants.
; help - prints the call procedure.
;
; Example:
;   find_off, vcen=225.,width=10.,vwidth=100,loc=[285.91,16.56]
;   off_loc
;
; Created by Dr. Kat Barger
;-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(help) then begin
   print,''
   print,'find_off, vcen=vcen, width=width, loc=loc, max_HI=max_HI, help=help'
   print,'vcen=center velocity of emission
   print,'loc=[glon,glat]'
   print,'max_HI=maximum HI expected for plotting purposes. Default 5e18 cm^-2'
   print,''
   return;
endif

if loc[0] lt 0 then loc[0]=loc[0]+360.

;if user doesn't pass stars keyword,
;assume that we should search for stars
if (size(stars,/type) eq 0) then stars=1

if NOT keyword_set(loc) then begin
   print,'Must specify loc=[glon,glat]'
   return;
endif
if keyword_set(loc) then begin
   if n_elements(loc) ne 2 then begin
      print,'Please specify loc properly. loc=[glon,glat]'
      return;
    endif
endif
loc_lon=loc[0]
loc_lat=loc[1]
if NOT keyword_set(width) then width=15.
if NOT keyword_set(vcen) then vcen=0
if NOT keyword_set(max_HI) then max_HI=5.0e18
if NOT keyword_set(vwidth) then vwidth=100.
if NOT keyword_set(smooth) then smooth=0

print,''
Print,'lon, lat: ',loc[0],loc[1],format='(A-15,f-8.2,f-8.2)'
Print,'Center velocity: ',vcen,format='(A-15,f8.2)'
print,''

loc_lon=loc[0]
loc_lat=loc[1]
lon_min=loc_lon-width
lon_max=loc_lon+width
;If lon_max gt 360. then begin
;   lon_max=lon_max-360.
;   if lon_min gt lon_max then begin
;      temp=lon_min
;      lon_min=lon_max
;      lon_max=temp
;   endif
;endif
lat_min=loc_lat-width
lat_max=loc_lat+width


both=0
if (lon_max gt 360) OR (lon_min lt 0) then begin
    hi_bridge_low=exthi((lon_min+360.) mod 360.,360.,lat_min,lat_max,/wham,/quiet,magellanic=magellanic)
    hi_bridge_high=exthi(0.0,(loc_lon+width) mod 360.,lat_min,lat_max,/wham,/quiet,magellanic=magellanic)
    hi_bridge=replicate(hi_bridge_low[0],n_elements(hi_bridge_low)+n_elements(hi_bridge_high))
    hi_bridge[0:n_elements(hi_bridge_low)-1]=hi_bridge_low
    hi_bridge[n_elements(hi_bridge_low):n_elements(hi_bridge_low)+n_elements(hi_bridge_high)-1]=hi_bridge_high
    both=1
endif else If (lon_max lt 360.) AND (both ne 1) then hi_bridge=exthi(lon_min,lon_max,lat_min,lat_max,/wham,/quiet,magellanic=magellanic) $
else if both ne 1 then hi_bridge=exthi(0,360,lat_min,lat_max,/wham,/quiet,magellanic=magellanic)

;subindex=where((Hi_Bridge.glon le lon_max) $
;           and (Hi_Bridge.glon ge lon_min) $
;           and (Hi_Bridge.glat lt lat_max) $
;           and (Hi_Bridge.glat ge lat_min))
;hi_bridge=hi_bridge(subindex)

hi_int=intmap(hi_bridge,vmin=vcen-vwidth/2.0,vmax=vcen+vwidth/2.0,/hi)* 1.8224e18

@kat_color

   if keyword_set(ps) and (NOT keyword_set(click)) then begin

      loc_string=strcompress(string(loc[0],format='(f8.2)')+'_'+string(loc[1],format='(f8.2)'),/re)
       name='LAB_HI_'+loc_string+'.eps'

       entry_device=!d.name
       set_plot,'PS'
       !p.font=0
       device,filename=name,bits_per_pixel=16,color=1
       device,/landscape,encapsulated=1, /helvetica
       device,xsize=10,xoffset=(11.0-10)*0.5,/inches

    !p.symsize=1.75
    !p.charthick=3.5
    !p.charsize=1.5
    !p.thick=9
    !p.position=[0.2,0.15,0.8,.9]

   endif else begin

    set_plot,'x'

   endelse   

!p.background=fsc_color('white')

    zmin = 0 & zmax = max_HI
    whammap,hi_bridge,0,0,hi_int,/useimage,$  
    smgrid = 2.649166613817E-01,scale = 1.25,/cbottom,$
    beamradius = 0.3,loncenter=loc[0],latcenter=loc[1],$
    ymargin=[10.5,5.5], xmargin=[8,8], missing = missing, _extra = extra,$
    zmin = zmin, zmax = zmax, $
    smooth = smooth,lon_labels=2,/xytitles,magellanic=magellanic

if keyword_set(magellanic) then begin
  gal2mag,loc_lon,loc_lat,ml,mb
  plots,-ml,mb,psym=4,symsize=3,color=fsc_color('blue'),thick=5
endif else $
plots,-loc_lon,loc_lat,psym=4,symsize=3,color=fsc_color('blue'),thick=5

if (stars eq 1) then begin

print,'Searching for bright stars...'
print,'Bright stars indicated with a +'

;The WHAM survey used 6 mag and 0.55 degrees as the cutoff
;removing stars
stars_flag=fltarr(n_elements(hi_bridge))
faint=6 
star_radius=0.55
for k=0, n_elements(hi_bridge)-1 do begin 
    command=strcompress('whamsao -gal -rad'+ string(star_radius) +' -faint '+string(faint)+''+string(hi_bridge(k).glon,hi_bridge(k).glat))
    for j=0, n_elements(command)-1 do begin
        spawn,command(j),result
        no=strsplit(result(7),/extract)
     endfor
    if no(0) ne 'No' then stars_flag(k) = 1
endfor

bad=where(stars_flag eq 1)

if keyword_set(magellanic) then begin
  gal2mag,hi_bridge[bad].glon,hi_bridge[bad].glat,ml,mb
  plots,-ml,mb,psym=1,color=fsc_color('black'),symsize=1.5,thick=2
endif else $
plots,-hi_bridge[bad].glon,hi_bridge[bad].glat,psym=1,color=fsc_color('black'),symsize=1.5,thick=2

endif

@kat_color

 colorbar, ncolors = !d.table_size-1, range = [zmin, zmax]/1.0e18, $
   maxrange = 254, $
   position = [!x.window[1]*1.05, !y.window[0], !x.window[1]*1.1, !y.window[1]], $
   title = 'Column Density [10!u18!n cm!u-2!n]', $
   format = '(f5.1)',VERTICAL=1,right=1,charsize=1.75


if keyword_set(blocks) then begin

      if size(blocks,/type) eq 7. then begin
          blocks=validate_extension(blocks,'dat')
          readcol,blocks,glon1234,glat1234,bnum1234,/silent
      endif else $
      readcol,'$HOME/WHAM/Bridge/Planning/magellanic_pointings.dat',glon1234,glat1234,bnum1234,/silent
      pointing1234=fltarr(3,n_elements(bnum1234))
      pointing1234[0,*]=glon1234
      pointing1234[1,*]=glat1234
      pointing1234[2,*]=bnum1234

   color_names= FSC_Color(/Names)
   loadct,35
   numberOfNumbersNeeded = 500.
   rng = Obj_New('RandomNumberGenerator', initialSeed)
   randomNumbers = rng -> GetRandomNumbers(numberOfNumbersNeeded)
   color_index = fix(randomNumbers*225.)+5

if size(blocks,/type) ne 7. then begin
   good_blknum=where(bnum1234 le 1234.)
   glon1234=glon1234[good_blknum]
   glat1234=glat1234[good_blknum]
   bnum1234=bnum1234[good_blknum]
endif

lon_min=loc[0]-width/2.0
lon_max=loc[0]+width/2.0
lat_min=loc[1]-width/2.0
lat_max=loc[1]+width/2.0


if (lon_min lt 0) and (lon_max gt 0) then begin

   subindex=where(((glon1234 le lon_max) $
              and (glon1234 ge 0)) $
              or ((glon1234 ge lon_min+360.) $
              and (glon1234 le 360.)))
   glon1234=glon1234[subindex]
   glat1234=glat1234[subindex]
   bnum1234=bnum1234[subindex]

  subindex=where((glat1234 le lat_max) $
              and (glat1234 ge lat_min))

   glon1234=glon1234[subindex]
   glat1234=glat1234[subindex]
   bnum1234=bnum1234[subindex]

endif


   bnum_index=bnum1234[uniq(bnum1234)]
   for j=0, n_elements(bnum1234[uniq(bnum1234)])-1 do begin
    bnum=bnum_index[j]
      pointings1234 = where(pointing1234[2, *] eq bnum)
      npoints1234=n_elements(pointings1234)
        glon = total(pointing1234[0, pointings1234], 1)
        glat = total(pointing1234[1, pointings1234], 1)
     plots,-glon,glat,color=fsc_color('black'),psym=1,thick=!p.thick+2
     plots,-glon,glat,color=color_index[randomu(seed)*63],psym=1

     xyouts,-median(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('black'),$
	/data,charsize=1.2,alignment=0.75,charthick=4
     xyouts,-median(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('white'),$
	/data,charsize=1.2,alignment=0.75,charthick=1

   endfor

endif

count=0
count_all=0
if keyword_set(large_blocks) then begin

      readcol,'$HOME/WHAM/Bridge/Planning/magellanic_pointings.dat',glon2000,glat2000,bnum2000,/silent
      pointing2000=fltarr(3,n_elements(bnum2000))
      pointing2000[0,*]=glon2000
      pointing2000[1,*]=glat2000
      pointing2000[2,*]=bnum2000

   color_names= FSC_Color(/Names)
   loadct,23
   numberOfNumbersNeeded = 200.
   rng = Obj_New('RandomNumberGenerator', initialSeed)
   randomNumbers = rng -> GetRandomNumbers(numberOfNumbersNeeded)
   color_index = fix(randomNumbers*200.)+5

   good_blknum=where(bnum2000 ge 2000.)
   glon2000=glon2000[good_blknum]
   glat2000=glat2000[good_blknum]
   bnum2000=bnum2000[good_blknum]

lon_min=loc[0]-width/2.0
lon_max=loc[0]+width/2.0
lat_min=loc[1]-width/2.0
lat_max=loc[1]+width/2.0

   subindex=where((glon2000 le lon_max) $
              and (glon2000 ge lon_min),count)

   if count ne 0 then begin

   glon2000=glon2000[subindex]
   glat2000=glat2000[subindex]
   bnum2000=bnum2000[subindex]

   subindex=where((glat2000 le lat_max) $
              and (glat2000 ge lat_min),count_all)

   if count_all ne 0 then begin

   glon2000=glon2000[subindex]
   glat2000=glat2000[subindex]
   bnum2000=bnum2000[subindex]

   bnum_index=bnum2000[uniq(bnum2000)]
   for j=0, n_elements(bnum2000[uniq(bnum2000)])-1 do begin
    bnum=bnum_index[j]
      pointings2000 = where(pointing2000[2, *] eq bnum)
      npoints2000=n_elements(pointings2000)
        glon = total(pointing2000[0, pointings2000], 1)
        glat = total(pointing2000[1, pointings2000], 1)
     oplot,-glon,glat,color=fsc_color('black'),psym=1,thick=!p.thick+2
     oplot,-glon,glat,color=color_index[randomu(seed)*63],psym=1

     xyouts,-avg(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('black'),$
	/data,charsize=1.2,alignment=0.75,charthick=4
     xyouts,-avg(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('white'),$
	/data,charsize=1.2,alignment=0.75,charthick=1
   endfor

   endif
   endif
 
   if (count eq 0) or (count_all eq 0) then begin
      print,' '
      print,'*** NO LARGE BLOCKS ****' 
   endif

endif

@kat_color

  if keyword_set(click) then begin
     curspect,hi_bridge,/hi1,xrange=[vcen-vwidth/2.0,vcen+vwidth/2.0],xstyle=1,magellanic=magellanic
 endif

   if keyword_set(ps) then begin

      !p.multi=0
      !p.thick=1
      !x.thick=1
      !y.thick=1
  
      device,/close_file
      set_plot,entry_device
      Close, /All
  
      set_plot,'x'

   endif  

if keyword_set(click) then begin
  print,' '
  print,'Run off_loc to click on the map and output the cursor location to determine the off position'
  print,' ' 
endif

end