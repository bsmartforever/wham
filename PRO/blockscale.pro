pro blockscale,date, calibfile, calib, ftsext = ftsext

	; Change of plans. I am going to read in the important info, feed it into readblock to get my tables.
	;Then this program will feed the calibrators for the night into transcale and have it spit out a scale factor
	;Then use that scale factor to scale ALL the blocks

	;I want to do the calibrators first so that I have the scale done and out of the way

	;blockscale, '/d/WHAM/bsmart/SMC/110104/ha', 'b2003', '110104', 'l_ori_list.txt', ftsext = 'ATMSUB'


	readcol,date,format='a',input_files

	
	n_dates=n_elements(input_files)

	;if there is only one file it doesn't bother with looping.
	if n_dates EQ 1 then begin

		;for i = num do begin	
		
			map = readblocklist('/d/wham/bsmart/SMC/'+input_files+'/ha/combo/blocklist.txt')
			; Right now each date will need its own block list and calibfile. Need to 

			unscalemap=map

			unscalemap.data=unscalemap.data/22.8
		
			transcale, '/d/wham/bsmart/SMC/'+input_files+'/ha/combo/'+calibfile, input_files, calib,scale, var

			;print, 'Date: ', input_files,' Scale: ', scale , ' Variance: ', var
		
			; possible to to map = readblocklist('./blocks.ha.lst'), ext='ATMSUB'. This will create a map of a lot of blocks. BUT ONLY DO THIS FOR A SINGLE night
			;Right now scaling can only be done for a single night, not multiple nights. 
		
			;I need to figure out a what to use readblock list but still do particular nights scaled properly. Will need more then one nights worth of
			;good scalers to do this.
		
			map.data=scale*map.data/22.8
		
			map.var=map.var*scale*scale/(22.8)^2

		;endif

	endif else begin

		;if you have multiple date files it will get taken care of here. Currently only works with l-ori as a calibrator and
		;doesn't average the scaling yet

		;Initialize map as a structure first

			map = readblocklist('/d/wham/bsmart/SMC/'+input_files[0]+'/ha/combo/blocklist.txt')

			unscalemap=map

			unscalemap.data=unscalemap.data/22.8
		
			transcale, '/d/wham/bsmart/SMC/'+input_files[0]+'/ha/combo/'+calibfile, input_files[0], calib,scale, var

			;print, 'Date: ', input_files[0],' Scale: ', scale , ' Variance: ', var
		
			map.data=scale*map.data/22.8
		
			map.var=map.var*scale*scale/(22.8)^2

			;this adds in all the subsequent dates

		for i=1,n_dates-1 do begin

			map_temp = readblocklist('/d/wham/bsmart/SMC/'+input_files[i]+'/ha/combo/blocklist.txt')

			unscaletemp=map_temp

			unscaletemp.data=map_temp.data/22.8

			unscalemap=[unscalemap,unscaletemp]
		
			transcale, '/d/wham/bsmart/SMC/'+input_files[i]+'/ha/combo/'+calibfile, input_files[i], calib,scale, var

			;print, 'Date: ' , input_files[i] , ' Scale: ', scale , ' Variance: ', var
		
			map_temp.data=scale*map_temp.data/22.8
		
			map_temp.var=map_temp.var*scale*scale/(22.8)^2

			map=[map,map_temp]
		endfor

	endelse

	@mycmap
	;loadct,3

	glon_min=min(map.glon)
	glon_max=max(map.glon)
	glat_min=min(map.glat)
	glat_max=max(map.glat)

		whammap, map, 125,100,beamrad=0.34,/lin,zmax=.2,zmin=0.000005, /gal2mag, $
		limits=[glon_min,glon_max, glat_min, glat_max], smooth=1

		;whammap, unscalemap, 125,100,beamrad=0.34,/lin,zmax=.5,zmin=0.000005, /gal2mag, $
		;limits=[glon_min,glon_max, glat_min, glat_max], smooth=0

		close, /all

	return

	end