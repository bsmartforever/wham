pro block_dates,$ 
	blocklist=blocklist, allblocks=allblocks,exptime=exptime,$
	starttime = starttime, startatdark=startatdark,$
	filename=filename,ps=ps
;
; pro block_dates
; 
; Purpose: 
;	To create a list of optimal dates to observe a specific blocks
;	Reads in a date and writes out the blocks which are best to osberve
;	Note that this program uses the David Nidever Gaussian decompositions from the LAB survey
;
; Input: 
;	[blocklist]	  - Feeds in the list of blocks we want the optimal date from
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
; Example for best blocks of a given night:
; block_dates,starttime=julday (11, 0, 2015, 2, 25 ) + 4./24 
;
; Created by Brianna Smart 02/2016
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Setup:
;
; Change this directory to match the path to 'msfinal2.sav' on your machine
dir='/d/data/hi/Nidever_Stream/'
;
; Change this to match the location and name of your file that contains
; the wham block pointing positions and block numbers.
; This file should contain many rows of "glon,glat,bnum" in it.
pointings_file='$HOME/PRO/KAT_PRO/pointings.dat'
;
; Setup postscript ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	if keyword_set(filename) OR keyword_set(PS) then device='PS' else device='X'

	if (NOT keyword_set(blocklist)) then blocklist=pointings_file

	if (NOT keyword_set(exptime)) then exptime=60.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This section will be for checking what blocks are best. This will read in ALL 
; possible blocks and gie out the best for the given date
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	IF keyword_set(startatdark) THEN starttime = darktime()

	IF keyword_set(startatdark) THEN begin
;MAKE SURE TO CHANGE THIS FOR THE ONE IN YOUR PATH!
	if (NOT keyword_set(blocks)) AND (NOT keyword_set(allblocks)) then blocks=0 $
		else begin
			readcol,blocklist,glon,glat,bnum,/silent

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
		endelse


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
				print, starttime
				eq2hor, ra, dec, dark_times[1], alt, az, obsname = 'ctio'
				zd_end_of_night = 90-alt
				print, dark_times[1]

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

END