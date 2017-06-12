pro create_ms_plots, mlon, mlat, width=width,$ 
	blocks=blocks, allblocks=allblocks,exptime=exptime,$
	starttime = starttime, startatdark=startatdark,$
	filename=filename,ps=ps
;
; pro create_ms_plots
; 
; Purpose: 
;	To create 2 plots Magellanic System 
;		1) column density and position map 
;		2) mlon vs vlsr plot
;	Note that this program uses the David Nidever Gaussian decompositions from the LAB survey
;
; Input: 
;	mlon/mlat     - Magellanic Stream Longitude/Latitude. Specifies the center position to plot.
;	[width]		  - Specifies the mlon range to plot. Note that the mlat range = 1/2 the width
;	[blocks]      - Outline the Magellanic block positions and label them (min block #7000)
;	[allblocks]	  - Outline ALL WHAM block positions and label them
;	[starttime]	  - Calculates the zenith distances for the specified night at the starttime to the end of darktime.
;				    Specify a start date and time, e.g., starttime=julday(11, 0, 2015, 2, 25 ) + 4./24 
;					This example is for Nov, 0, 2015 at 2:25 a.m.. The 4./24 is for CST time zone 				
;	[startatdark] - Calculate the zenith distances for tonight at the start and end of darktime.
;	[filename] 	  - File name of output postscript file. Does not require the extension. 
;				    e.g., filename='test.eps' or filename='test' will both produce a file named 'test.eps'
;				    Does not need to be combined with the [ps] keyword
;	[ps] 		  - Postscript flag to create plot with default name 'ms_plots.eps'
;
; Dependencies: 
;				  - CHANGE THE SETUP TO MATCH YOUR MACHINE!!! SEE BELOW IN SETUP!!!
;				  - David Nidever's gal2mag program
;				  - msfinal2.sav, this is David Nidever's Gaussian decompositons of the LAB survey
;				  - file containing the wham block pointing positions and block numbers.
;					e.g., pointings.dat
;				  - Uses Coyote programs
;				  - @kat_color, uses Dr. Kat Barger's color table 
;				  - Uses validate_extension.pro
;				  - Uses Dr. Kat Barger's version of whammap.pro. 
;					Not compatible with the current one in /d/wham/pro, which doesn't graph in
;					Magellanic Stream coordinantes. 
;				  - Needs WHAM programs
;				  - Uses psconfig in idlutils. idlutils is a collection of IDL utilities useful for 
;					astronomical applications, developed by an assortment of folks over the years. 
;					Various pipelines for the Sloan Digital Sky Survey and other projects rely on tools from this product.
;					Find some info on idlutils here: https://www.sdss3.org/dr8/software/idlutils.php
;					tar balls for idlutils can be found here: http://spectro.princeton.edu/tarballs/
;
;
; Example:
; create_ms_plots,-50,-5,width=20,/blocks,starttime=julday (11, 0, 2015, 2, 25 ) + 4./24 
;
; Created by Dr. Kat Barger 07/2015
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Setup:
;
; Change this directory to match the path to 'msfinal2.sav' on your machine
dir='/d/engels2/bsmart/data/hi/Nidever_Stream/'
;
; Change this to match the location and name of your file that contains
; the wham block pointing positions and block numbers.
; This file should contain many rows of "glon,glat,bnum" in it.
pointings_file='$HOME/PRO/KAT_PRO/pointings.dat'
;
; Setup postscript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

if keyword_set(filename) OR keyword_set(PS) then device='PS' else device='X'
if strmatch(device,'ps',/fold_case) then begin

   set_plot,device 
   if keyword_set(filename) then $
      name=validate_extension(filename,'eps') $
   else name='ms_plots.eps'

   entry_device=!d.name

if keyword_set(portrait) then undefine,xsize,ysize,landscape

if n_elements(xsize) then if xsize eq 0 then undefine, xsize
if n_elements(ysize) then if ysize eq 0 then undefine, ysize
keywords = PSConfig(/nogui,xsize=xsize,ysize=ysize,Encapsulated=1,$
  Filename=name,FontType=-1,landscape=landscape,/helvetica)
  thisDevice = !D.Name
  Set_Plot, 'PS'
  Device, _Extra=keywords

   !x.thick=5
   !y.thick=5
   !p.charthick=4
   charsize=1.1
   if (!p.thick le 2) AND (NOT keyword_set(thick)) then !p.thick=5 $
   else if (keyword_set(thick)) then !p.thick=thick else !p.thick=5

endif else begin

   set_plot,device
   if (!p.thick ge 4) AND (NOT keyword_set(thick)) then !p.thick=2
   if (!x.thick ge 4) AND (NOT keyword_set(thick)) then !x.thick=2
   if (!y.thick ge 4) AND (NOT keyword_set(thick)) then !y.thick=2
   charsize=1.2

endelse

if (NOT keyword_set(exptime)) then exptime=60.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

IF keyword_set(startatdark) THEN starttime = darktime()

;MAKE SURE TO CHANGE THIS FOR THE ONE IN YOUR PATH!
if (NOT keyword_set(blocks)) AND (NOT keyword_set(allblocks)) then blocks=0 $
else begin
	readcol,pointings_file,glon,glat,bnum,/silent

  	if keyword_set(starttime) then begin
  		observatory, 'ctio', obs
  		dark_times = get_dark_times(starttime - obs.tz/24.0)
		starttime = dark_times[0]
		;; Average block 'per pointing' overhead: 
		pointing_overhead = 6.2
 		;; Average block startup overhead:
		block_overhead = 18
		plan = 0.0D  ;; plan accumulator in units of day(s)
  	endif 
  	;Look at all the blocks, or just the Magellanic Stream Blocks
  	if keyword_set(allblocks) then minblock=1 else minblock=7000
  	good_blocks=where(bnum ge minblock)
  	bnum=bnum[good_blocks]
  	glon=glon[good_blocks]
  	glat=glat[good_blocks]

  	pointing=fltarr(3,n_elements(bnum))
    pointing[0,*]=glon
    pointing[1,*]=glat
    pointing[2,*]=bnum
endelse 

if (NOT keyword_set(width)) then width=40.

mlon_min=mlon-width/2.
mlon_max=mlon+width/2.
mlat_min=mlat-width/4.
mlat_max=mlat+width/4.

if (NOT keyword_set(hi)) then begin
  hi_funky=restore_var(dir+'msfinal2.sav')
  hi=replicate({mlon:0.0,mlat:0.0,data:0.0,vlsr:0.0},n_elements(hi_funky.mlon))
  hi.mlon=hi_funky.mlon
  hi.mlat=hi_funky.mlat
  hi.data=hi_funky.par0*1.82240e+18
  hi.vlsr=hi_funky.par1
  undefine,hi_funky
endif
num=n_elements(hi)
i=0
while i lt num do begin
  loc=where((hi[i].mlon eq hi.mlon) AND (hi[i].mlat eq hi.mlat),nloc)
  hi[i].data=total(hi[loc].data)
  if nloc gt 1 then begin
     remove,loc[1:*],hi
     num=num-(nloc-1)
  endif
   i++
endwhile

mapindex=where((hi.mlon le mlon_max-0.25) $
           and (hi.mlon ge mlon_min+0.25) $
           and (hi.mlat lt mlat_max-0.25) $
           and (hi.mlat ge mlat_min+0.25),num_good)

hi=hi(mapindex)

mvel_min=float(round(min(hi.vlsr)))
mvel_max=float(round(max(hi.vlsr)))

hi_min=min(hi.data)
hi_max=max(hi.data)

mvel_min=float(round(min(hi.vlsr)))
mvel_max=float(round(max(hi.vlsr)))

;Only look at the mlon and mlat region of interest

!p.position=[0,0.60,1,0.95]

@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,256) & g=frebin(g,256) & b=frebin(b,256)
   r[0:1]=255 & g[0:1]=255 & b[0:1]=255 
   r[254]=0 & g[254]=0 & b[254]=0 
   r[255]=255 & g[255]=255 & b[255]=255 
   tvlct, r, g, b

zmin=alog10(hi_min) & zmax=alog10(hi_max)
whammap,hi,0,0,alog10(hi.data),/useimage,/magellanic,$
    charsize=1.0,charthick=charthick,$
    /linear,loncenter=avg([mlon_min,mlon_max]),$
    zmin = zmin, zmax = zmax,$
    scale = 1.2,/cbottom,$
    ymargin=[10.5,5.5], xmargin=[8,8],$
    beamradius = 0.3,$;lat_labels=2,$
    limits=[mlon_min,mlon_max,mlat_min,mlat_max],smooth=0,$
    /noborder,glondel=fix(width/2.),glatdel=fix(width/4.)

if keyword_set(blocks) then begin
; Add block stuff ;;;;;;;;;;;;;;;
	
	  gal2mag,glon,glat,mlon,mlat
  
	  subindex_labels=where((mlon le mlon_max) $
             and (mlon ge mlon_min) $
             and (mlat le mlat_max) $
             and (mlat ge mlat_min),subnum_good)
  
	  pointing=pointing[*,subindex_labels]
	  bnum=bnum[subindex_labels]
	  mlon=mlon[subindex_labels]
	  mlat=mlat[subindex_labels]
	  n_lines=subnum_good

	  time = 0D
	  labels=MAKE_ARRAY(n_lines,3)
	  bestarray=MAKE_ARRAY(2,n_lines)
	  nul1=[0]
	  nul2=[0]

	if keyword_set(starttime) then begin
		
		;Note that NP=Number of Points of the block that is within the region specified.

		print, "Action", "NP", "Dur.", "ZD Start", "ZD End", 'MLON', 'MLAT', $
		    format = '(A, T40,A4,A10,A15,A9,A8,A10)' 
		  print, strjoin(replicate('-', 105))
		 
		  print, systime(/utc, /julian) - obs.tz/24.0, format = '("Current time is ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2), " CLST")'
		  print, starttime - obs.tz/24.0, format = '("Observation time: ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2), " CLST", /)'
		  plan = 0.0D  ;; plan accumulator in units of day(s)
		
		  print, "**** START DARK TIME (CLST UT-" + string(obs.tz, format='(I02)') + $
		  	") ****", $
		  string(dark_times[0]-obs.tz/24.0,format='(C(CHI2.2,":",CMI2.2,":",CSI2.2))'),$
		    format = '(A, T40, 4X, 2X, A8)'
		  print, "**** NIGHT OF ", string(dark_times[0]-0.5, format='(C(cmoi2.2,"/",cdi2.2,"/",cyi4.2))'), " ****", $
		  	format = "(A, A, A,/)"
		  date=string(dark_times[0]-0.5, format='(C(cmoi2.2,"/",cdi2.2,"/",cyi4.2))')
		  stime=string(starttime - obs.tz/24.0, format = '("    ", C(CHI2.2, ":", CMI2.2, ":", CSI2.2)/)')
		  deg = '"'+String("260B)+'"'
	endif
	
	cgloadct,7,/silent
	;;****************Here we are in a for loop that is going through all the blocks*********
	  i=0
	  while(i le subnum_good-1) do begin
		
		pointings = where(pointing[2, *] eq bnum[i],npoints)
		npoints=n_elements(pointings)  
	
		; Calculate zenith distance for each pointing individually and report max
		;	Using mean or median glon and glat doesn't effectively represent the block
		;	when block wraps around l=0/360
		 
	
				glon_block = total(pointing[0, pointings], 1)
		        glat_block = total(pointing[1, pointings], 1)
				gal2mag,glon_block,glat_block,mclon,mclat
		        euler, glon_block, glat_block, ra, dec, 2
	
		   if (keyword_set(starttime)) then begin     
	
		   	eq2hor, ra, dec, starttime, alt, az, obsname = 'ctio'
			zd_start_of_night = 90-alt
			eq2hor, ra, dec, dark_times[1], alt, az, obsname = 'ctio'
			zd_end_of_night = 90-alt

		if (min(zd_start_of_night) ge 60.) AND (i eq 0) then begin
			print,''
			print,' *** No blocks with ZD_start_of_night lt 60. ***'
			print,''
		endif
	 
		;******* Sets colors for block depending on ZD_start_of_night
		
					if (max(zd_start_of_night) GT 70) THEN BEGIN
					c=cgcolor('orchid')
					ENDIF
					if (max(zd_start_of_night) GT 50) && (max(zd_start_of_night) LT 70) THEN BEGIN
					c=cgcolor('aqua')
					ENDIF
					if (max(zd_start_of_night) GT 35) && (max(zd_start_of_night) LT 50)  THEN BEGIN
					c=cgcolor('blu4')
					ENDIF
					if (max(zd_start_of_night) GT 25) && (max(zd_start_of_night) LT 35)  THEN BEGIN
					c=cgcolor('blu6')
					ENDIF
					if(max(zd_start_of_night) LT 25)  THEN BEGIN
					c=cgcolor('dodger blue')
					ENDIF
			
			IF (max(zd_start_of_night) LT 60) THEN BEGIN
			
					;Duration of observation, block or pointings
					time = (npoints * (exptime + pointing_overhead) + $
			                block_overhead) / 60.0 
		
			        obstime = ( indgen(npoints) * (exptime+pointing_overhead) + $
			        	block_overhead ) / (60.0*60.0*24) ; (in days)
		
			        print, 'b'+strcompress(string(fix(bnum[i])),/re), npoints, $
			          time, max(zd_start_of_night), max(zd_end_of_night), median([mclon]), median([mclat]), $
			          format = '(A, T40, I4, 2X, F10.2,F10.2, '+deg+',F10.2, '+deg+', F+10.2,'+deg+', F+9.2,'+deg+')'
				 	bestarray=[[bestarray],[i,max(zd_start_of_night)]]
		
			 		 plan = plan + time/60.0/24.0

			ENDIF 
		endif 


	  	if keyword_set(blocks) AND (npoints ge 15) then begin
	  			;*******More plotting here. Sets colors for block depending on ZD_start_of_night
	  		if (NOT keyword_set(starttime)) then c=cgcolor('black')
	  		;if (NOT keyword_set(starttime)) then c=cgcolor('orchid')
			n=n_elements(pointings)
			PLOTs, -[mlon[pointings[0]]-.5,mlon[pointings[n-1]]+.5], [mlat[pointings[0]]-.5,mlat[pointings[0]]-.5],  color=c, thick=.5
			PLOTs, -[mlon[pointings[0]]-.5,mlon[pointings[n-1]]+.5], [mlat[pointings[n-1]]+.5,mlat[pointings[n-1]]+.5],  color=c, thick=.5
			PLOTs, -[mlon[pointings[n-1]]+.5,mlon[pointings[n-1]]+.5], [mlat[pointings[0]]-.5,mlat[pointings[n-1]]+.5],  color=c, thick=.5
			PLOTs, -[mlon[pointings[0]]-.5,mlon[pointings[0]]-.5], [mlat[pointings[0]]-.5,mlat[pointings[n-1]]+.5],  color=c, thick=.5
	  	endif
	
		;Write block numbers on map, but only if at least 15 sightlines are contained within the plotting region
		if npoints ge 15 then begin
      		xyouts,-avg(mlon[pointings]),avg(mlat[pointings])-0.5,$
      			string(fix(bnum[i]-7000,type=2)),color=cgcolor('black'),$
	  			/data,charsize=charsize*0.65,alignment=0.75,charthick=charthick
	  	endif

			i=i+npoints
	endwhile

	if keyword_set(starttime) then begin
  		spawn,'echo -ne "\033[30m"'
  		print, "****** END DARK TIME ******", $
  		string(dark_times[1]-obs.tz/24.0,format='(C(CHI2.2,":",CMI2.2,":",CSI2.2))'),$
    	format = '(/, A, T40, 4X, 2X, A8)'

    	print, "****** TOTAL DARK HOURS ******",(dark_times(1)-dark_times(0))*24.0,format='(A-41,f8.1)'
	endif
	
endif
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

xyouts,avg(!p.position[[0,2]]),0.975,'Magellanic Stream Longitude (Degrees)',/normal,color=cgcolor('black'),align=0.5,charsize=charsize,charthick=charthick
xyouts,0.06,avg(!p.position[[1,3]]),'Magellanic Stream Latitude',/normal,color=cgcolor('black'),align=0.5,orientation=90,charsize=charsize,charthick=charthick
xyouts,0.09,avg(!p.position[[1,3]]),'(Degrees)',/normal,color=cgcolor('black'),align=0.5,orientation=90,charsize=charsize,charthick=charthick


@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,255) & g=frebin(g,255) & b=frebin(b,255)
   r[253:254]=r[252] & g[253:254]=g[252] & b[253:254]=b[252] 
   tvlct, r, g, b


MinData=alog10(hi_min) & MaxData=alog10(hi_max)
     title='log( N!DHI!N / cm!U-2!N )'
     colorbar, ncolors = !d.table_size-1, range = [mindata, maxdata], $
       position = [0.89, 0.5+0.025, 0.915, 0.95+0.025], $
       VERTICAL=1,right=1,charsize=charsize,charthick=charthick,$
       _extra=extra,color=color,title=title


;Only look at the mlon and v_lsr region of interest



!p.position=[0.1,0.1,0.875,0.5]

@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,255) & g=frebin(g,255) & b=frebin(b,255)
   r[253:254]=r[252] & g[253:254]=g[252] & b[253:254]=b[252] 
   tvlct, r, g, b


xrange=[max(hi.mlon),min(hi.mlon)]
plot,hi.mlon,hi.vlsr,/NODATA,/noerase,color=cgcolor('black'),$
	xtitle='Magellanic Stream Longitude (Degrees)',$
	ytitle='LSR Velocity (km/s)',$
	xminor=1,yminor=1,xstyle=2,xrange=xrange,$
	charthick=!p.charthick,charsize=charsize

@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,255) & g=frebin(g,255) & b=frebin(b,255)
   r[253:254]=r[252] & g[253:254]=g[252] & b[253:254]=b[252] 
   tvlct, r, g, b

color_arr=sym_color(data_array=alog10(hi.data))
for i=0,num_good-1 do plots,hi[i].mlon,hi[i].vlsr,color=color_arr[i],psym=symcat(16),symsize=0.75

;Sade regions that are within -50<vls<+50 km/s in red. This region will be especially confused with the Milky Way

polyfill,[!x.crange[0],!x.crange[0],!x.crange[1],!x.crange[1]],$
		[-50,50,50,-50],$
		 ;color=fsc_color('red4'),linestyle=0,thick=0.5,$
		 color=fsc_color('red'),linestyle=0,thick=0.5,$
		 spacing=0.25,orientation=-45
oplot,[!x.crange[0],!x.crange[1]],[-50,-50],linestyle=0,thick=!p.thick,color=fsc_color('black')
oplot,[!x.crange[0],!x.crange[1]],[50,50],linestyle=0,thick=!p.thick,color=fsc_color('black')

@kat_color
   tvlct, r, g, b, /get
   r = r[25:255] & g = g[25:255] & b = b[25:255]
   r=frebin(r,255) & g=frebin(g,255) & b=frebin(b,255)
   r[253:254]=r[252] & g[253:254]=g[252] & b[253:254]=b[252] 
   tvlct, r, g, b


MinData=alog10(hi_min) & MaxData=alog10(hi_max)
     title='log( N!DHI!N / cm!U-2!N )'
     colorbar, ncolors = !d.table_size-1, range = [mindata, maxdata], $
       position = [0.89, 0.1, 0.915, 0.5], $
       VERTICAL=1,right=1,charsize=charsize,charthick=charthick,$
       _extra=extra,color=color,title=title


; close postscript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    !p.multi=0

if keyword_set(device) then begin
   if (device eq 'ps') or (device eq 'PS') then begin

    device,/close_file
    set_plot,entry_device
    Close, /All
    
    endif

    set_plot,'x'

endif    

cleanplot,/silent ;To reset and clean all idl plotting structures (!p, !x, !y)
close,/all

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

end