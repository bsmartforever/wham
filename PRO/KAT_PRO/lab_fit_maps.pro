pro lab_fit_maps, loc=loc, width=width, max_HI=max_HI,$ 
	blocks=blocks, survey_blocks=survey_blocks, $
	smooth=smooth, magellanic=magellanic, ps=ps,$ 
	stars=stars

;
;
;
;
; E.g., lab_fit_maps,loc=[-25,0],width=50,/magellanic,/block
;
; NOTE: These LAB HI Survey fits come from Nidever+2008 (2008ApJ...679..432N). 
; If you use this program, please thank Nidever for providing us with his fits.
;
; Created by Dr. Kat Barger 05/2014
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;HI col variable: hi_col
;HI vel variable: hi_vel

;Change this to where Nidever's fits files lives on your machine
dir='/d/data/hi/Nidever_Stream/'
;Change this to the location of your *pointings.dat file on your machine
block_file='$HOME/WHAM/Bridge/Planning/magellanic_pointings.dat'

;Assumes that you have the system variable !k2cm2 defined
if n_elements(!k2cm2) eq 0 then defsysv,'!k2cm2',1.8224e18

if (NOT keyword_set(loc)) then begin
	print,''
	print,'*** Please set loc ***'
	print,'*** loc=[lon,lat] ***'
	print,''
	return
endif
if (loc[0] lt 0) AND (NOT keyword_set(magellanic)) then loc[0]=loc[0]+360.

;if user doesn't pass stars keyword,
;assume that we should search for stars
if (size(stars,/type) eq 0) then stars=0

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
if NOT keyword_set(smooth) then smooth=0
if NOT keyword_set(max_HI) then max_HI=18.5

print,''
Print,'lon, lat: ',loc[0],loc[1],format='(A-15,f-8.2,f-8.2)'
print,''

loc_lon=loc[0]
loc_lat=loc[1]
lon_min=loc_lon-width
lon_max=loc_lon+width
lat_min=loc_lat-width
lat_max=loc_lat+width

   if keyword_set(ps) then begin

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
    window,0
    window,1
    wset,0

   endelse   

;This has all of the HI fit componets, which means that there will be multiple along the sight line. 
;Only going to show the total integrated HI column density along each sight line.
if (NOT keyword_set(hi_col)) then begin
  hi_col_funky=restore_var(dir+'msfinal2.sav')
  hi_col=replicate({glon:0.0,glat:0.0,mlon:0.0,mlat:0.0,data:0.0},n_elements(hi_col_funky.mlon))
  hi_col.mlon=hi_col_funky.mlon
  hi_col.mlat=hi_col_funky.mlat
  hi_col.data=hi_col_funky.par0*!k2cm2 ;*hi_col_funky.par2*sqrt(2.0*!pi)
  mag2gal,hi_col.mlon,hi_col.mlon,glon,glat
  hi_col.glon=glon
  hi_col.glat=glat
  undefine,hi_col_funky

; if (NOT keyword_set(Magellanic)) then begin
;   subindex=where((hi_col.glon le lon_max) $
;          and (hi_col.glon ge lon_min) $
;          and (hi_col.glat lt lat_max) $
;          and (hi_col.glat ge lat_min))
; endif else begin
;	subindex=where((hi_col.mlon le lon_max) $
;	         and (hi_col.mlon ge lon_min) $
;	         and (hi_col.mlat lt lat_max) $
;	         and (hi_col.mlat ge lat_min))
; endelse
; hi_col=hi_col(subindex)
endif
num=n_elements(hi_col)
i=0
while i lt num do begin
  index=where((hi_col[i].mlon eq hi_col.mlon) AND (hi_col[i].mlat eq hi_col.mlat),nindex)
  hi_col[i].data=total(hi_col[index].data)
  if nindex gt 1 then begin
     remove,index[1:*],hi_col
     num=num-(nindex-1)
  endif
   i++
endwhile

;Only going to include the average HI velocity along each sight line
if (NOT keyword_set(hi_vel)) then begin
  hi_vel_funky=restore_var(dir+'msfinal2.sav')
  hi_vel=replicate({glon:0.0,glat:0.0,mlon:0.0,mlat:0.0,v0:0.0},n_elements(hi_vel_funky.mlon))
  hi_vel.mlon=hi_vel_funky.mlon
  hi_vel.mlat=hi_vel_funky.mlat
  hi_vel.v0=hi_vel_funky.par1
  mag2gal,hi_vel.mlon,hi_vel.mlon,glon,glat
  hi_vel.glon=glon
  hi_vel.glat=glat
  undefine,hi_vel_funky

;  if (NOT keyword_set(Magellanic)) then begin
;  	subindex=where((hi_vel.glon le lon_max) $
;  	         and (hi_vel.glon ge lon_min) $
;  	         and (hi_vel.glat lt lat_max) $
;  	         and (hi_vel.glat ge lat_min))
;  endif else begin
;	subindex=where((hi_vel.mlon le lon_max) $
;	         and (hi_vel.mlon ge lon_min) $
;	         and (hi_vel.mlat lt lat_max) $
;	         and (hi_vel.mlat ge lat_min))
;  endelse
;  hi_vel=hi_vel(subindex)
endif

;;; Plot HI column 

if (NOT keyword_set(ps)) then wset,0

;Save original color tables
tvlct, r_orig, g_orig, b_orig, /get
@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,255) & g=frebin(g,255) & b=frebin(b,255)
   r[0:1]=255 & g[0:1]=255 & b[0:1]=255 
   r[253:254]=r[252] & g[253:254]=g[252] & b[253:254]=b[252] 
   tvlct, r, g, b


;!p.background=fsc_color('white')

    zmin = 17.5 & zmax = max_HI
    whammap,hi_col,0,0,alog10(hi_col.data),/useimage,$ 
    smgrid = 2.649166613817E-01,scale = 1.25,/cbottom,$
    charsize=1.0,charthick=1,$
    beamradius = 0.3,loncenter=loc[0],latcenter=loc[1],$
    ymargin=[10.5,5.5], xmargin=[8,8], missing = missing, _extra = extra,$
    zmin = zmin, zmax = zmax, /linear,limits=[lon_min,lon_max,lat_min,lat_max],$
    smooth = smooth,lon_labels=2,/xytitles,magellanic=magellanic,/noborder


@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,255) & g=frebin(g,255) & b=frebin(b,255)
   r[0:1]=255 & g[0:1]=255 & b[0:1]=255 
   r[253:254]=r[252] & g[253:254]=g[252] & b[253:254]=b[252] 
   tvlct, r, g, b

 colorbar, ncolors = !d.table_size-4, bottom=3,$
   range = [zmin, zmax], $
   position = [0.20, 0.86, 0.87, 0.89], $
   title = 'Log(HI Column Density / cm!U-2!N)',$
   format = '(f8.1)',VERTICAL=0,right=1,charsize=1.25


if (stars eq 1) then begin

print,'Searching for bright stars...'
print,'Bright stars indicated with a +'

;The WHAM survey used 6 mag and 0.55 degrees as the cutoff
;removing stars
stars_flag=fltarr(n_elements(hi_col))
faint=6 
star_radius=0.55
for k=0, n_elements(hi_col)-1 do begin 
    command=strcompress('whamsao -gal -rad'+ string(star_radius) +' -faint '+string(faint)+''+string(hi_col(k).glon,hi_col(k).glat))
    for j=0, n_elements(command)-1 do begin
        spawn,command(j),result
        no=strsplit(result(7),/extract)
     endfor
    if no(0) ne 'No' then stars_flag(k) = 1
endfor

bad=where(stars_flag eq 1)

if keyword_set(magellanic) then begin
  gal2mag,hi_col[bad].glon,hi_col[bad].glat,ml,mb
  plots,-ml,mb,psym=1,color=fsc_color('black'),symsize=1.5,thick=2
endif else $
  plots,-hi_col[bad].glon,hi_col[bad].glat,psym=1,color=fsc_color('black'),symsize=1.5,thick=2
endif


if keyword_set(survey_blocks) then begin

      if size(blocks,/type) eq 7. then begin
          blocks=validate_extension(blocks,'dat')
          readcol,blocks,glon1234,glat1234,bnum1234,/silent
      endif else $
      readcol,block_file,glon1234,glat1234,bnum1234,/silent
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

if (keyword_set(magellanic)) then mag2gal,loc[0],loc[1],loc_block0,loc_block1
block_lon_min=loc_block0-width/2.0
block_lon_max=loc_block0+width/2.0
block_lat_min=loc_block1-width/2.0
block_lat_max=loc_block1+width/2.0


if (block_lon_min lt 0) and (block_lon_max gt 0) then begin

   subindex=where(((glon1234 le block_lon_max) $
              and (glon1234 ge 0)) $
              or ((glon1234 ge block_lon_min+360.) $
              and (glon1234 le 360.)))
   glon1234=glon1234[subindex]
   glat1234=glat1234[subindex]
   bnum1234=bnum1234[subindex]

  subindex=where((glat1234 le block_lat_max) $
              and (glat1234 ge block_lat_min))

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
	
   	 	if keyword_set(Magellanic) then begin
	
   	 		gal2mag,glon,glat,mlon,mlat
	
   	 		oplot,-mlon,mlat,color=fsc_color('black'),psym=1,thick=!p.thick+2
   	 		oplot,-mlon,mlat,color=color_index[randomu(seed)*63],psym=1
   	 	
   	 		xyouts,-avg(mlon),avg(mlat)-0.5,string(fix(bnum,type=2)),color=fsc_color('black'),$
				/data,charsize=1.2,alignment=0.75,charthick=4
   	 		xyouts,-avg(mlon),avg(mlat)-0.5,string(fix(bnum,type=2)),color=fsc_color('white'),$
				/data,charsize=1.2,alignment=0.75,charthick=1
	
   	 	endif else begin   
   	 		oplot,-glon,glat,color=fsc_color('black'),psym=1,thick=!p.thick+2
   	 		oplot,-glon,glat,color=color_index[randomu(seed)*63],psym=1
	
   	 		xyouts,-avg(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('black'),$
				/data,charsize=1.2,alignment=0.75,charthick=4
   	 		xyouts,-avg(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('white'),$
				/data,charsize=1.2,alignment=0.75,charthick=1
 		endelse
   	endfor

endif


count=0
count_all=0
if keyword_set(blocks) then begin

      readcol,block_file,glon2000,glat2000,bnum2000,/silent
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

;if (keyword_set(magellanic)) then mag2gal,loc[0],loc[1],loc_block0,loc_block1
;block_lon_min=loc_block0-width/2.0
;block_lon_max=loc_block0+width/2.0
;block_lat_min=loc_block1-width/2.0
;block_lat_max=loc_block1+width/2.0
;
;   subindex=where((glon2000 le block_lon_max) $
;              and (glon2000 ge block_lon_min),count)
;
;   if count ne 0 then begin
;
;   glon2000=glon2000[subindex]
;   glat2000=glat2000[subindex]
;   bnum2000=bnum2000[subindex]
;
;   subindex=where((glat2000 le block_lat_max) $
;              and (glat2000 ge block_lat_min),count_all)
;
;   if count_all ne 0 then begin

;   glon2000=glon2000[subindex]
;   glat2000=glat2000[subindex]
;   bnum2000=bnum2000[subindex]

   bnum_index=bnum2000[uniq(bnum2000)]
   	for j=0, n_elements(bnum2000[uniq(bnum2000)])-1 do begin
   	 bnum=bnum_index[j]
   	   pointings2000 = where(pointing2000[2, *] eq bnum)
   	   npoints2000=n_elements(pointings2000)
   	     glon = total(pointing2000[0, pointings2000], 1)
   	     glat = total(pointing2000[1, pointings2000], 1)
	
   	 	if keyword_set(Magellanic) then begin
	
   	 		gal2mag,glon,glat,mlon,mlat
	
   	 		oplot,-mlon,mlat,color=fsc_color('black'),psym=1,thick=!p.thick+2
   	 		oplot,-mlon,mlat,color=color_index[randomu(seed)*63],psym=1
   	 	
   	 		xyouts,-avg(mlon),avg(mlat)-0.5,string(fix(bnum,type=2)),color=fsc_color('black'),$
				/data,charsize=1.2,alignment=0.75,charthick=4
   	 		xyouts,-avg(mlon),avg(mlat)-0.5,string(fix(bnum,type=2)),color=fsc_color('white'),$
				/data,charsize=1.2,alignment=0.75,charthick=1
	
   	 	endif else begin   
   	 		oplot,-glon,glat,color=fsc_color('black'),psym=1,thick=!p.thick+2
   	 		oplot,-glon,glat,color=color_index[randomu(seed)*63],psym=1
	
   	 		xyouts,-avg(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('black'),$
				/data,charsize=1.2,alignment=0.75,charthick=4
   	 		xyouts,-avg(glon),avg(glat)-0.5,string(fix(bnum,type=2)),color=fsc_color('white'),$
				/data,charsize=1.2,alignment=0.75,charthick=1
 		endelse
   	endfor

   endif
   ;endif
 
   if (count eq 0) or (count_all eq 0) then begin
      print,' '
      print,'*** NO LARGE BLOCKS ****' 
   endif

;endif


;;; Plot HI velocity

if (NOT keyword_set(ps)) then wset,1

loadct,33

zmin=min(hi_vel.v0) & zmax=max(hi_vel.v0)
whammap,hi_vel,0,0,hi_vel.v0,/useimage,magellanic=magellanic,$
    charsize=1.0,charthick=1,$
    /linear,loncenter=loc[0],latcenter=loc[1],$
    zmin = zmin, zmax = zmax,$
    scale = 1.2,/cbottom,limits=[lon_min,lon_max,lat_min,lat_max],$
    ymargin=[10.5,5.5], xmargin=[8,8],$
    beamradius = 0.3,$;lat_labels=2,$
    smooth=smooth,$
    /noborder,/xytitles

loadct,33
 colorbar, ncolors = !d.table_size-1, range = [zmin,zmax], $
   ;maxrange = 254, $
   position = [0.20, 0.86, 0.87, 0.89], $
   title = 'LSR Velocity (km s!U-1!N)',$
   format = '(I4)',VERTICAL=0,right=1,charsize=1.25

   if keyword_set(ps) then begin

      !p.multi=0
      !p.thick=1
      !x.thick=1
      !y.thick=1
  
      device,/close_file
      set_plot,entry_device
      Close, /All
  
      set_plot,'x'
 
   endif else wset,0

;Save restore color tables
tvlct, r_orig, g_orig, b_orig

end