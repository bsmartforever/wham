;+
;				CF_EDIT
;
; Routine to edit the CALFUSE Intermediate time-tag data files (IDF files)
;
; CALLING SEQUENCE:
;	cf_edit
;
; HISTORY
;	version 1, D. Lindler, May 2003
;	Nov 2003, Bernard Godard, now prints the total exptime for the 
;		image according to the events selection flags and the 
;		user's good time intervals.
;	Dec 2003, added wavelength registration, ttag analysis, and 
;		weights for histogram image construction.
;	Dec 2003, removed pinhole aperture from menu, added wavelength offsets
;		to menu for selecting spectra to register, populate exposure
;		time field in xregister_1d.
;	21 Dec 2003, Van Dixon: Remove underscore from idf and bpm
;                               filenames.
;       Jan 2004, B. Godard: added computation of Y CENTROIDS
;                            NEVENTS keyword now updated in output file
;			New way to compute image scaling factor (histogram)
;	version 2.0, V. Dixon, 16 Feb 2004
;       version 2.1, B. Godard, 17 Feb 2004: FILENAME and ROOTNAME keywords
;                                            now updated
;       version 2.2, B. Godard, 7 Apr 2004: Switched FEC_CNT_RATE
;                                           AIC_CNT_RATE in
;                                           cf_edit_gti_selection to match 
;                                           GTI Selection menu 
;	version 2.3, V. Dixon, 6 May 2004: Decreased default spectral binning
;						from 0.02 to 0.013
;						Angstrom.
;       version 2.4, B. Godard, 12 May 2004: Fixed dayflag bug
;       version 2.5, B. Godard, 3 June 2004: Modified to handle EXP_STAT
;                                            keyword and HIST files.
;	version 2.6, V. Dixon, 11 June 2004: Switch from TrueColor to PseudoColor
;						if running on a Mac.
;       version 2.7, B. Godard, 14 June 2004: bug fix (syntax error in 
;                                             cf_edit_gti_selection)
;       version 2.8, B. Godard, 17 June 2004: bug fix (exptimes index in 
;                                             cf_edit_register);
;       version 2.9, V. Dixon, 26 July 2004: Modify wavelength ranges of extracted
;                                             spectra.
;	version 3.0, B. Godard, 25 Oct 2004: Update EXPNIGHT header keyword
;	version 3.1, V. Dixon, 29 March 2005: If presented with an empty IDF file,
;					issue a warning message and continue.
;	version 3.2, V. Dixon, 12 July 2005: comment out keyword "decomp=0"
;					in final call to DEVICE
;	version 3.3, V. Dixon, 15 Feb. 2007: Replace findfile with file_search.
;	version 3.4, V. Dixon, 16 Feb. 2007: Replace line_edit with line_edit2.
;-
;================================================================ CF_EDIT_EVENT
;
; CF_EDIT event handler
;
pro cf_edit_event,event
	common cf_edit_common,info,image,little_image,input,selection

	widget_control,event.id,get_uvalue=uvalue
	tvlct,info.rsave,info.gsave,info.bsave

	case uvalue of
	'FILE': begin
		case event.value of
;
; read time-tag file
;
		1: begin
		   cf_edit_read,info,input,selection
		   cf_edit_make_image,info,input,image,selection
 		   cf_edit_display,info,input,image,little_image,/new
		   end
;
; write time-tag file
;
		2: IF (input.nfiles EQ 0) THEN BEGIN
                   message = "NO DATA"
                   istat = dialog_message(message,/error, dialog_parent=info.base)
                END ELSE BEGIN 
                   cf_edit_write,info,input,selection		
                END
;
; default parameters
;
		3: begin
		   xbin = info.xbin
		   ybin = info.ybin
		   cf_edit_default,info
		   if (info.xbin ne xbin) or (info.ybin ne ybin) then begin
		       cf_edit_make_image,info,input,image,selection
 		       cf_edit_display,info,input,image,little_image,/new
		   end
		   end
			
;
; Postscript output
;
		5: cf_edit_ps,info,input
		6: cf_edit_ps,info,input,/reversed
		7: cf_edit_ps,info,input,/color
;
; Exit
;
		8: begin
			cf_edit_free,input
			info = 0
			image = 0
		   	widget_control,event.top,/destroy
		   end
		endcase
	      end
	      
	'COLORS': begin
		xloadct,/modal,group=event.top
		tvlct,rsave,gsave,bsave,/get
		info.rsave = rsave
		info.gsave = gsave
		info.bsave = bsave
		if !d.n_colors gt 255 then $
				cf_edit_display,info,input,image,little_image
		end
	'DISPLAY': begin
		   cf_edit_make_image,info,input,image,selection
 		   cf_edit_display,info,input,image,little_image,/new
		end	      
	'SCALE':begin 
		cf_edit_display,info,input,image,little_image
		end
	'MIN_FIELD': cf_edit_display,info,input,image,little_image
	'MAX_FIELD': cf_edit_display,info,input,image,little_image
	
	'FREEZE': begin
		widget_control,info.freeze,get_value=v
		if v eq 'Freeze Min/Max' then newv = 'UnFreeze Min/Max' $
					 else newv = 'Freeze Min/Max'
		widget_control,info.freeze,set_value=newv
		end
	'OVERLAY': begin
		cf_edit_specify_overlay,info
		cf_edit_display,info,input,image,little_image
		end
	'EVENT_SELECTION': begin
		cf_edit_event_selection, selection, group_leader=info.base
		cf_edit_make_image,info,input,image,selection
 		cf_edit_display,info,input,image,little_image,/new
		end
	'GTI SELECTION': begin
		cf_edit_gti_selection,info,input,event.value,selection, $
			group_leader=info.base
		cf_edit_make_image,info,input,image,selection
 		cf_edit_display,info,input,image,little_image,/new
		end
	'EXTRACT SPECTRUM': begin
		cf_edit_extract,info,event.value,input,selection
		end
	'RESET': begin
		widget_control,info.min_field,set_value=float(info.omin)
		widget_control,info.max_field,set_value=float(info.omax)
		cf_edit_display,info,input,image,little_image
		end		
	'ZOOM': begin
		x = info.xoffzoom + 128/info.zoom_factor
		y = info.yoffzoom + 128/info.zoom_factor
		info.zoom_factor = fix(strmid(event.value,12,2))		
		info.xoffzoom = (x - 128/info.zoom_factor)>0
		info.yoffzoom = (y - 128/info.zoom_factor)>0
		cf_edit_zoom,info,image,zoom
		cf_edit_scale,info,info.zoom_id,zoom
		end
	'BIG_WINDOW': cf_edit_cursor,event,info,input,image,little_image,'BIG'

	'LITTLE_WINDOW': cf_edit_cursor,event,info,input,image, $
				little_image,'LITTLE'

	'ZOOM_WINDOW': cf_edit_cursor,event,info,input,image,$
				little_image,'ZOOM'
	'PLOT': if event.value eq 5 then cf_edit_plot_pha,info,input  $
				    else cf_edit_lineplot,info,event
	'BACKGROUND': begin
		info.overlay(1) = 1
		cf_edit_display_overlay,info,input
		cf_edit_background,info,input,event
		end
	'REGISTER': begin
		cf_edit_register,info,event.value,input,selection
		end
	'TTAG': begin
		   xend = (info.xoffzoom + (256+info.zoom_factor-1)/ $
			info.zoom_factor-1)<(info.ns-1)
	 	   yend = (info.yoffzoom + (256+info.zoom_factor-1)/ $
		   	info.zoom_factor-1)<(info.nl-1)
		   xr = [info.xoffzoom,xend]
		   yr = [info.yoffzoom,yend]
		   cf_edit_xyttag,info,xr,yr
		   cf_edit_ttag_select,info,input,selection,x,y,t
		   cf_edit_ttag,input.files(0),x,y,t,info.xbin,info.ybin, $
				xr,yr,group=event.top		 
		end
	'HEADER': cf_edit_header,event,input
        'CALCULATE Y_CENTROID': begin                            
                                  cf_edit_calculate_y_centroid,info,event.value,input,selection
                                  cf_edit_display,info,input,image,little_image
                                end

	else:
	endcase
end
;============================================================= CF_EDIT_DEFAULT
;
; Routine to edit the default parameters
;
;
pro cf_edit_default,info


	main = widget_base(group=info.base,/modal,/column, $
		title='Default Parameters')
	
	base = widget_base(main,/row)

	label = widget_label(base,value='Image Binning:   ')
	xbin_base = widget_droplist(base,uvalue='XBIN',title='X:', $
			value=['1','2','4','8','16','32'])
	ybin_base = widget_droplist(base,uvalue='YBIN',title='Y:', $
			value=['1','2','4','8'])
	deltaw_base = cw_field(main,title='Extracted wavelength spacing: ', $
				xsize=10,uvalue='DELTAW',value=info.deltaw, $
				/float,/row)
	
	base = widget_base(main,/row)
	label = widget_label(base,value='Extracted spectral units: ')
	
	unit_base = cw_bgroup(base,['Counts','Flux (erg/cm2/sec/'+string("305B)+')'], $
		set_value=info.flux,/exclusive,uvalue='UNITS',/row)
	
	cal_base = cw_field(main,title='Cal File Directory: ', $
				xsize=45,uvalue='DELTAW', $
				value=info.caldir,/row)
	button = widget_button(main,value='DONE',uvalue='DONE')

	widget_control,main,/realize

	binvals = [1,2,4,8,16,32]
	widget_control,xbin_base,set_droplist=(where(binvals eq info.xbin))[0]
	widget_control,ybin_base,set_droplist=(where(binvals eq info.ybin))[0]
	
	ptr = ptr_new({xbin_base:xbin_base, ybin_base:ybin_base, $
		unit_base:unit_base, $
		cal_base:cal_base, deltaw_base:deltaw_base, $
		xbin:info.xbin, ybin:info.ybin, deltaw:info.deltaw, $
		caldir: info.caldir, flux:info.flux})
	widget_control,main,set_uvalue=ptr
	xmanager,'cf_edit_default',main
	info.xbin = (*ptr).xbin
	info.ybin = (*ptr).ybin
	info.flux = (*ptr).flux
	info.deltaw = (*ptr).deltaw
	info.caldir = (*ptr).caldir
	ptr_free,ptr
end
pro cf_edit_default_event,event
	widget_control,event.top,get_uvalue=ptr
	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
		'XBIN': (*ptr).xbin = 2^event.index
		'YBIN': (*ptr).ybin = 2^event.index
		'UNITS': (*ptr).flux = event.value
		'DONE': begin
			widget_control,(*ptr).cal_base,get_value=v
			(*ptr).caldir = v
			widget_control,(*ptr).deltaw_base,get_value=v
			(*ptr).deltaw = v>0.005
			widget_control,event.top,/destroy
			end
		else:
	endcase
end


;================================================================= CF_EDIT_READ
;
; Routine to read time-tag files
;
pro cf_edit_read,info,input,selection
;
;
; get list of files
;
	filter = input.directory+'*idf.fit'
	if !version.os_family eq 'Windows' then filter = '*idf.fit'
	files = dialog_pickfile(title='Select FUSE IDF file(s)',/must_exist, $
		filter=filter,/multiple,dialog_parent = info.base)
	if files(0) eq '' then begin
		status = 'No File Selected'
		return
	endif
	fdecomp,files(0),disk,directory,name
	widget_control,/hourglass
;
; release memory from previous observations
;
	cf_edit_free,input
	widget_control,info.basebar,sensitive=0
	widget_control,info.basebar2,sensitive=0
;
; skip empty IDF files
;
	nfiles = n_elements(files)
	good = intarr(nfiles)
	for i=0,nfiles-1 do begin
		header = headfits(files(i), EXTEN=1, /silent)
		IF strtrim(sxpar(header,'TFORM1')) EQ '0E' THEN begin
			good[i] = 1
        	        message = "File "+strtrim(strmid(files(i), $
                                strpos(files(i),'/',/REVERSE_SEARCH)+1))+' contains no data.'
                        istat = dialog_message(message,/information, dialog_parent=info.base)
                ENDIF
	endfor
	g = where(good eq 0, n)
	if (n eq 0) then begin
		status = 'No Valid Files'
		return
	endif
	files = files[g]
	nfiles = n
;
; create pointer structure to hold data
;
	input = {	nfiles:nfiles, $
			files:files, $
			detector:'', $
			expstart:dblarr(nfiles), $
			timestart:dblarr(nfiles), $
			directory:directory, $
			header:ptrarr(nfiles), $
			event:ptrarr(nfiles), $
			gti:ptrarr(nfiles), $
			mask:ptrarr(nfiles), $
			timeline:ptrarr(nfiles), $
			event_header:ptr_new(), $
			gti_header:ptr_new(), $
			timeline_header:ptr_new(), $
			chid_cal:'', $
			ycent:fltarr(8), $
			ylow:ptrarr(8), $
			yhigh:ptrarr(8), $
			ycent_changed:intarr(8), $
			woffset_lif:fltarr(nfiles), $
			woffset_sic:fltarr(nfiles), $
			instmode:'TTAG'}

	for i=0,nfiles-1 do begin
		widget_control,info.message,set_v='Reading '+files(i)
		data = mrdfits(files(i),0,header,/silent,/fscale)

		IF strtrim(sxpar(header,'instmode')) EQ 'HIST' THEN BEGIN
                   input.instmode='HIST'
                ENDIF
                
		exp_stat = sxpar(header, 'EXP_STAT', comment=comment)
                IF strtrim(exp_stat) NE 0 THEN BEGIN
                   message = strcompress('EXP_STAT = '+string(exp_stat)+' ('+comment+') in '+ $
			strmid(files(i), strpos(files(i),'/',/REVERSE_SEARCH)+1))
                   istat = dialog_message(message,/information, dialog_parent=info.base)
                ENDIF

		if i eq 0 then begin
			input.detector = strtrim(sxpar(header,'detector'))
		    end else begin
		    	if strtrim(sxpar(header,'detector')) ne $
						input.detector then begin
				istat = dialog_message('All files must be ' + $
					'the same detector segment',/error, $
					dialog_parent=info.base)
				cf_edit_free,input
				widget_control,info.basebar,sensitive=0
				widget_control,info.basebar2,sensitive=0
				widget_control,/hourglass
				return
			end
		end
		input.expstart[i] = sxpar(header,'expstart')
		input.header[i] = ptr_new(header,/no_copy)
		
		data = mrdfits(files(i),1,header,/silent,/fscale)
		input.event[i] = ptr_new(data,/no_copy)
		if i eq 0 then input.event_header = ptr_new(header,/no_copy)
		
		data = mrdfits(files(i),2,header,/silent,/fscale)
		input.gti[i] = ptr_new(data,/no_copy)
		if i eq 0 then input.gti_header = ptr_new(header,/no_copy)
		
		data = mrdfits(files(i),3,header,/silent,/fscale)
		input.mask[i] = ptr_new(replicate(1B,n_elements(data.time)), $
					/no_copy)
		input.timeline[i] = ptr_new(data,/no_copy)
		if i eq 0 then input.timeline_header = ptr_new(header,/no_copy)
	end
	widget_control,info.message,set_v=' '
;
; sort by time
;
	sub = sort(input.expstart)
	input.files = input.files(sub)
	input.expstart = input.expstart(sub) 
	input.header = input.header(sub)
	input.event = input.event(sub)
	input.gti = input.gti(sub)
	input.mask = input.mask(sub)
	input.timeline = input.timeline(sub)		
	for i=0,nfiles-1 do input.timestart[i] =  $
		(input.expstart[i]-input.expstart[0])*24.0*60.0*60.0
;
; create selection array
;
	daynight = sxpar(*(input.header(0)),'daynight')
	if !err lt 0 then daynight = 'BOTH'
	case strtrim(daynight) of
		'DAY': dayflag = 1
		'NIGHT': dayflag = 0
		'BOTH': dayflag = 2
		else: dayflag = 2
	endcase
	phalow = sxpar(*(input.header(0)),'phalow')
	if !err lt 0 then phalow = 0
	phahigh = sxpar(*(input.header(0)),'phahigh')
	if !err lt 0 then phahigh = 31
	maxtime = input.timestart(nfiles-1) + $
		max((*input.timeline(nfiles-1)).time) + 1
	selection = {	timeflags:[intarr(7),dayflag], $
			use_gti:1, $
			pha_range:[phalow,phahigh] $
;			, time_range:[0.0,maxtime], $
;			full_time_range:[0.0,maxtime] $
        }

        IF input.instmode EQ 'HIST' THEN BEGIN
           selection.timeflags(3) = 2 ; BURSTS
           selection.timeflags(5) = 2 ; SAA
           selection.timeflags(6) = 2 ; LIMB ANGLE
        ENDIF
                        


	widget_control,info.basebar,sensitive=1
	widget_control,info.basebar2,sensitive=1
	if input.instmode eq 'HIST' then $
		widget_control,info.ttag_button,sensitive=0
;
; extract background regions from header
;
	info.nback = sxpar(*(input.header(0)),'bkgd_num')
	for i=0,info.nback-1 do begin
	     info.bpos(i*2) = sxpar(*(input.header(0)),'bkg_min'+strtrim(i,2))
	     info.bpos(i*2+1) = sxpar(*(input.header(0)),'bkg_max'+strtrim(i,2))
	end
;
; extract Ycent values from header
;
        FOR i=0,6 DO BEGIN
           input.ycent(i) = sxpar(*(input.header(0)),'ycent'+strtrim(i+1,2))
           info.ycents(i+1) =  input.ycent(i)
        ENDFOR

	widget_control,/hourglass
end


;================================================================= CF_EDIT_FREE
;
; Routine to free pointer variables
;
pro cf_edit_free,input
	if input.nfiles gt 0 then begin
		directory = input.directory
		 ptr_free,input.header
		 ptr_free,input.event
		 ptr_free,input.gti
		 ptr_free,input.mask
		 ptr_free,input.timeline
		 ptr_free,input.event_header
		 ptr_free,input.gti_header
		 ptr_free,input.timeline_header
		 ptr_free,input.ylow
		 ptr_free,input.yhigh
		 input = {nfiles:0,directory:directory}
	end
END
;=================================================== CF_EDIT_GET_Y_CENTROID
;
; 
;
;
pro cf_edit_get_y_centroid,info, title, channel


	main = widget_base(group=info.base,/modal,/column)
	
	base = widget_base(main,/row)

	label = widget_label(base,value=title)
	
	ycents_base = cw_field(main,title='Y CENTROID: ', $
				xsize=10,uvalue='YCENTS',value=info.ycents(channel), $
				/float,/row)
	
	
	

	button = widget_button(main,value='DONE',uvalue='DONE')
        
	widget_control,main,/realize

	
	
	
	ptr = ptr_new({ ycents_base:ycents_base, $
		 ycents:info.ycents(channel) })
	widget_control,main,set_uvalue=ptr
	xmanager,'cf_edit_get_y_centroid',main

	info.ycents(channel) = (*ptr).ycents
	
	ptr_free,ptr
end
pro cf_edit_get_y_centroid_event,event
	widget_control,event.top,get_uvalue=ptr
	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
		'DONE': begin
			
			widget_control,(*ptr).ycents_base,get_value=v
			(*ptr).ycents = v>0.005
			widget_control,event.top,/destroy
			end
		else:
	endcase
end

;================================================== CF_EDIT_CALCULATE_Y_CENTROID
;
;
;
pro cf_edit_calculate_y_centroid,info,menu_number,input,selection
;

  
   
   
   m1 = (menu_number-1)/5
   m2 = menu_number-5*(m1)-2
   
   channel = ([1,2,3,5,6,7])[m1]
   ycent_mode =  (["Target events only","Airglow events only","Default","User select"])[m2]
   channel_title = (['','LiF HIRS','LiF MDRS','LiF LWRS','LiF PINH', $
		     'SiC HIRS','SiC MDRS','SiC LWRS','SiC PINH'])[channel]
   
  
  
   
   
   IF (m2 EQ 2) THEN BEGIN
      ; default
      
      cf_edit_get_ext_regions,info,input
      info.ycents_qual(channel) = 1
      info.ycents(channel) = info.ycents_default(channel-1)
      message = channel_title+" DEFAULT Y CENTROID = "+string(info.ycents(channel))
      istat = dialog_message(message,/information, dialog_parent=info.base)
      
      GOTO, the_end
   ENDIF

   IF (m2 EQ 3) THEN BEGIN
      ; user select

      cf_edit_get_y_centroid,info, channel_title, channel
      info.ycents_qual(channel) = 3

      GOTO, the_end
   ENDIF
   
   IF (m2 LT 2) THEN BEGIN

      select_byte=m2*2
      quality = 3-m2

      total_weight = 0.0d
      weighted_y_centroid = 0.0d
      
      FOR i=0,input.nfiles-1 DO BEGIN
         
         
         ; select events using time flags and GTI mask
         cf_edit_select,input,i,selection,mask
         good = where(mask,ngood)
         IF ngood EQ 0 THEN GOTO,nexti
        
         
         
         FOR j=0L, ngood-1 DO BEGIN
            IF ( (*input.event(i)).channel(good(j)) EQ channel) THEN BEGIN
               IF ( ((*input.event(i)).loc_flgs(good(j)) AND 2b) EQ select_byte) THEN BEGIN
                  total_weight = total_weight + (*input.event(i)).weight(good(j))
                  weighted_y_centroid = weighted_y_centroid + (*input.event(i)).weight(good(j)) * (*input.event(i)).y(good(j))    
               ENDIF
            ENDIF
            
         ENDFOR

nexti:
      ENDFOR

         IF m2 THEN message = " AIRGLOW EVENTS " ELSE message = " TARGET EVENTS " 
          
         IF (total_weight GT 0.0d) THEN BEGIN
            info.ycents(channel) = weighted_y_centroid/total_weight 
            info.ycents_qual(channel) = quality
            message = channel_title+message+ "Y CENTROID = "+string(info.ycents(channel))
            istat = dialog_message(message,/information, dialog_parent=info.base)
         ENDIF ELSE BEGIN
            
            message = channel_title + message + "Y CENTROID : UNDEF (No Data)"
            istat = dialog_message(message,/error,dialog_parent=info.base)
         ENDELSE

        
      
      
   ENDIF
   
the_end:

  
	
   return
   
end
;=========================================================== CF_EDIT_MAKE_IMAGE
;
; Routine to construct an image from the time-tag list(s)
;
pro cf_edit_make_image,info,input,image,selection
;
; determine type of image
;	0 = raw
;	1 = FARF
;	2 = Final
;	3 = wave/Y-final
;
	widget_control,/hourglass
	image_type = widget_info(info.display_base,/droplist_select)
	xbin = info.xbin
	ybin = info.ybin
	ns = 16384L/xbin
	nl = 1024L/ybin
	image = lonarr(ns*nl)

        totalexptime=0
;
; loop on images
;
	for i=0,input.nfiles-1 do begin

;
; select events using time flags and GTI mask
;
		exptime=0
		cf_edit_select,input,i,selection,mask,exptime=exptime
		totalexptime=totalexptime+exptime
		
		good = where(mask,ngood)
		if ngood eq 0 then goto,nexti

		case image_type of
		    0: begin
		    	x = (*input.event[i]).xraw[good]
			y = (*input.event[i]).yraw[good]
		       end
		    1: begin
		    	x = (*input.event[i]).xfarf[good]
			y = (*input.event[i]).yfarf[good]
		       end
		    2: begin
		    	x = (*input.event[i]).x[good]
			y = (*input.event[i]).y[good]
		       end
		    3: begin
		        if i eq 0 then coef=[[0d0,0d0],[0d0,0d0]]
			cf_edit_wtox,detector,input,i,good,coef,x
			y = (*input.event[i]).y[good]
		       end
		endcase
		index = round((y/ybin))*ns + round(x/xbin)
		inrange = where((x ge 0) and (x lt 16384) and (y gt 0) and $
				(y lt 1024), n)
		if n gt 0 then begin
		    if input.instmode ne 'HIST' then begin
			    image = histogram(index(inrange), $
					input=image,min=0,max=ns*nl-1)
			end else begin
			    cf_edit_drizzle,image,index, $
					(*input.event[i]).weight
		    end
		end		
nexti:
	end
	widget_control,info.exptime_field,set_value=float(totalexptime)
	image = reform(image,ns,nl,/overwrite)
	info.ns = ns
	info.nl = nl
	widget_control,/hourglass
end
;========================================================= CF_EDIT_WTOX
;
; Routine to convert wavelengths to x positions for the display
;
pro cf_edit_wtox,detector,input,i,good,coef,x
	
;
; determine dispersion coefficients if not already done
;
    	channel = (*input.event[i]).channel
	lif = where((channel ge 1) and (channel le 4),nlif)
	sic = where((channel ge 5),nsic)
	if coef(0,0) eq 0 and (nlif gt 2) then begin
		x = (*input.event[i]).x[lif]
		w = (*input.event[i]).lambda[lif]
		w = float(temporary(w))
		index = where((x gt 3000) and (x lt 11000) ,n)
		if n gt 2 then begin
			x = x(index)
			w = w(index)
			c = poly_fit(double(w),x,1,fit)
			coef(0,0) = c(0)
			coef(1,0) = c(1)
		end
	end

	if coef(0,1) eq 0 and (nsic gt 2) then begin
		x = (*input.event[i]).x[sic]
		w = (*input.event[i]).lambda[sic]
		index = where((x gt 3000) and (x lt 11000),n)
		if n gt 2 then begin
			x = x(index)
			w = w(index)
			c = poly_fit(double(w),x,1,fit)
			coef(0,1) = c(0)
			coef(1,1) = c(1)
		end

	end
;
; convert wavelength to x position
;
	w = (*input.event[i]).lambda(good)
	channel = channel(good)
	lif = where((channel ge 1) and (channel le 4),nlif)
	sic = where((channel ge 5),nsic)
	x = lonarr(n_elements(w))
	if (coef(0,0) ne 0.0) and (nlif gt 0) then $
		x(lif) = round(coef(0,0) + coef(1,0)*w(lif))
	if (coef(0,1) ne 0.0) and (nsic gt 0) then $
		x(sic) = round(coef(0,1) + coef(1,1)*w(sic))
end

;============================================================== CF_EDIT_DISPLAY
; 
; Routine to set up and display image all three windows
;
pro cf_edit_display,info,input,image,little_image,new=new
;
; process new image?
;
	widget_control,/hourglass
	if keyword_set(new) then begin		;new image?
		s = size(image) & ns = s(1) & nl = s(2)
		info.ns = ns
		info.nl = nl
		
                ;x1 = round(ns*0.06)
		;x2 = ns-x1-1
		;info.omin = min(image(x1:x2,*),max=imax)
		;info.omax = imax

                IF (max(image) LT 1) THEN BEGIN
                   info.omax = 1
                ENDIF ELSE BEGIN
                   image_hist = histogram(image, MIN=1)
                   image_total = total(image_hist)
                   current_sum = 0
                   i = 0
                   WHILE (current_sum LT 0.98*image_total) DO BEGIN
                      current_sum = current_sum+image_hist(i)
                      i = i+1
                   ENDWHILE
                   info.omax = i
                ENDELSE

                info.omin = 0

                

		widget_control,info.freeze,get_value=v
		if v eq 'Freeze Min/Max' then begin
			widget_control,info.min_field,set_value=float(info.omin)
			widget_control,info.max_field,set_value=float(info.omax)
		end
		nsout = 1024
		nlout = 128
		little_image = frebin(image,nsout,nlout)
		nxsize = ns
		widget_control,info.big_window,draw_xsize=nxsize,draw_ysize=nl, $
			scr_xsize=750<(ns+30),scr_ysize=542<(nl+30)
	end
	widget_control,info.message,set_value=' '
;
; display big image
;
	cf_edit_scale,info,info.big_id,image
;
; display little image
;	
	cf_edit_scale,info,info.little_id,little_image
;
; display zoom image
;
	cf_edit_zoom,info,image,zoom
	cf_edit_scale,info,info.zoom_id,zoom
;
; update overlay
;
	cf_edit_display_overlay,info,input
;
; update positions
;
	cf_edit_position,info

	wset,info.big_id
	widget_control,/hourglass
end


;============================================================= CF_EDIT_POSITION
;
; Routine to report regions displayed
;
pro cf_edit_position,info

;
; big window
;
	widget_control,info.big_window,get_draw_view=v
	xr = [v(0),(v(0)+744)<(info.ns-1)]
	yr = [v(1),(v(1)+514)<(info.nl-1)]
	cf_edit_xyttag,info,xr,yr
	widget_control,info.big_position,set_value=strtrim(xr(0),2)+':'+ $
		strtrim(xr(1),2)+'  '+strtrim(yr(0),2)+':'+ $
		strtrim(yr(1),2)
;
; zoom window
;
	xend = (info.xoffzoom + $
		(256+info.zoom_factor-1)/info.zoom_factor-1)<(info.ns-1)
	yend = (info.yoffzoom + $
		(256+info.zoom_factor-1)/info.zoom_factor-1)<(info.nl-1)
	xr = [info.xoffzoom,xend]
	yr = [info.yoffzoom,yend]
	cf_edit_xyttag,info,xr,yr
	widget_control,info.zoom_position,set_value=strtrim(xr(0),2)+':'+ $
		strtrim(xr(1),2)+'  '+strtrim(yr(0),2)+':'+ $
		strtrim(yr(1),2)
	return
	end
;=============================================================== CF_EDIT_XYTTAG
;
; Routine to convert back and forth from binned and raw timetag positions
;
pro cf_edit_xyttag,info,x,y,to_binned=to_binned,frac=frac
;
	if n_elements(to_binned) eq 0 then to_binned = 0
	if n_elements(frac) eq 0 then frac = 0
	xbin = info.xbin
	ybin = info.ybin
	if to_binned then begin
		if frac then begin
			x = (x - (xbin-1)/2.0)/xbin
			y = (y - (ybin-1)/2.0)/ybin
		   end else begin
		   	x = long(x)/xbin
			y = long(y)/ybin
		end
	   end else begin
	   	if frac then begin
			x = float(x)*xbin + (xbin-1)/2.0	
			y = float(y)*ybin + (ybin-1)/2.0
		   end else begin
		   	x = long(x)*xbin
			y = long(y)*ybin
		end
	end
return
end
; ================================================================ CF_EDIT_SCALE
;
; Routine to scale and display an image
;
pro cf_edit_scale,info, window_id, image
	

	widget_control,info.min_field,get_value=imin
	widget_control,info.max_field,get_value=imax
	i = widget_info(info.scale_base,/droplist_select)

	case i of 
		0: pic = bytscl(image,min=imin,max=imax,top=!d.n_colors)
		1: pic = bytscl(sqrt((image-imin)>0),min=0, $
					max=sqrt(imax-imin),top=!d.n_colors)
		2: begin
			tmin=imax/1e4
			pic = bytscl(alog10((image-imin)>tmin), $
					min=alog10(tmin), $
					max=alog10(imax-imin),top=!d.n_colors)
		   end
		3: pic = hist_equal(image,minv=imin,maxv=imax, $
					top=!d.n_colors)
	endcase

	wset,window_id
	erase
	tv,pic
end
;================================================================= CF_EDIT_ZOOM
;
; ROUTINE TO CREATE ZOOMED IMAGE 
;
pro cf_edit_zoom,info,image,zoom
	zoom_factor = info.zoom_factor
	xoff = info.xoffzoom
	yoff = info.yoffzoom
	s = size(image) & ns = s(1) & nl = s(2)
	if (xoff lt 0) or (yoff lt 0) or $
	   (xoff ge ns-1) or (yoff ge nl-1) then begin
	   	zoom = fltarr(256,256)
		return
	end 
	xend = (xoff + (256+zoom_factor-1)/zoom_factor-1)<(ns-1)
	yend = (yoff + (256+zoom_factor-1)/zoom_factor-1)<(nl-1)
	zoom = rebin(image(xoff:xend,yoff:yend), $
				(xend-xoff+1)*zoom_factor, $
				(yend-yoff+1)*zoom_factor,/samp)
	cf_edit_position,info
end
;=============================================================== CF_EDIT_CURSOR
;
; Routine to process cursor events
;
pro cf_edit_cursor,event,info,input,image,little_image,window

;
; cursor down?
;
	if event.press gt 0 then info.down = 1
	if event.release gt 0 then info.down = 0
;
; convert to image coordinates
;
	xin = event.x
	yin = event.y
	cf_edit_convert,info,xin,yin,window,x,y
	
	xx = x
	yy = y
	widget_control,info.val_field,set_value=float(image(xx,yy))
	cf_edit_xyttag,info,xx,yy
	widget_control,info.x_field,set_value=xx
	widget_control,info.y_field,set_value=yy
;
; update zoom window from big window position
;
	if (window eq 'BIG') and (event.press ge 1) and $
	   (info.cursor_state eq 'NONE') then begin
		info.xoffzoom = round(x - 128/info.zoom_factor)>0
		info.yoffzoom = round(y - 128/info.zoom_factor)>0
		cf_edit_zoom,info,image,zoom
		cf_edit_scale,info,info.zoom_id,zoom
	end	
;
; update zoom window from zoom window position
;
	if (window eq 'ZOOM') and (event.press ge 1) and $
	   (info.cursor_state eq 'NONE') then begin
		info.xoffzoom = round(x - 128/info.zoom_factor)>0
		info.yoffzoom = round(y - 128/info.zoom_factor)>0
		cf_edit_zoom,info,image,zoom
		cf_edit_scale,info,info.zoom_id,zoom
	end	
;
; update big window position for little window event
;
	if (window eq 'LITTLE') and (info.down eq 1) and $		
	   (info.cursor_state eq 'NONE') then begin
		x1 = (x-350)>0
		y1 = (y-256)>0
		widget_control,info.big_window,set_draw_view=[x1,y1]
		cf_edit_position,info
	end
;
; line or row sum Plots
;
	if info.cursor_state eq 'PLOT' then $
		cf_edit_lineplot,info,event,image,x,y,window
;
; background regions
;
	if info.cursor_state eq 'BACKGROUND' then $
		cf_edit_background,info,input,event,x,y,window,$
						image,little_image
end
	
	
	
;============================================================== CF_EDIT_CONVERT
;
; Routine to convert x,y coordinates from screen to data and vice versa
;
pro cf_edit_convert,info,xin,yin,window,xout,yout,to_screen=to_screen
;
; Inputs: xin, yin, window
;	window = 'big','little', or 'zoom'
; Outputs: xout, yout
; Keyword: /to_screen - if supplied coordinates are converted from
;			data to screen coord, otherwise conversion is
;			for screen to data.
;
	ns = info.ns
	nl = info.nl
	zoom_factor = info.zoom_factor
;
; conversion from screen to data
;
	if keyword_set(to_screen) then begin

		case strupcase(window) of
			'BIG': begin
				xout = xin
				yout = yin
				end
			'LITTLE': begin
				xfactor = ns/1024.
				yfactor = nl/128.
				xout = fix(xin/xfactor)
				yout = fix(yin/yfactor)
				end
			'ZOOM': begin
				xout = (xin - xoffzoom)*zoom_factor + $
							zoom_factor/2
				yout = (yin - yoffzoom)*zoom_factor + $
							zoom_factor/2
				end
		end
	    end else begin
;
; conversion for screen to data
;
		case window of
			'BIG': begin
				xout = xin
				yout = yin
				end
			'LITTLE': begin
				xfactor = ns/1024.
				yfactor = nl/128.
				xout = fix(xin*xfactor+xfactor/2)
				yout = fix(yin*yfactor+yfactor/2)
				end
			'ZOOM': begin
				xout = long(xin/zoom_factor) + info.xoffzoom
				yout = long(yin/zoom_factor) + info.yoffzoom
				end
		end
		xout = xout>0<(ns-1)
		yout = yout>0<(nl-1)
	end
	return
end
;==================================================================  CF_EDIT_PS
;
; Routine to generate postscript output files
;
pro cf_edit_ps,info,input,color=color,reversed=reversed


	if n_elements(color) eq 0 then color=0
;
; get output file
;
	file = dialog_pickfile(file='idl.ps',filter='*.ps',/write, $
			dialog_parent=info.base)
	if file eq '' then return	;no file selected
;
; get images to be displayed
;
	wset,info.little_id & pic1 = tvrd()
	wset,info.zoom_id & pic2 = tvrd()
	wset,info.big_id
	widget_control,info.big_window,get_draw_view=v
	ns = info.ns
	nl = info.nl
	pic3 = tvrd(v(0),v(1),745<(ns-v(0)),515<(nl-v(1)))
;
; rescale to 0 to 255
;
	nc = (!d.n_colors-1) < 255
	scale = 255/float(nc)
	pic1 = byte(pic1*scale+0.5)
	pic2 = byte(pic2*scale+0.5)
	pic3 = byte(pic3*scale+0.5)
;
; reverse
;
	if keyword_set(reversed) then begin
		pic1 = 255b-pic1
		pic2 = 255b-pic2
		pic3 = 255b-pic3
	end
;
; get color table
;
	if color then tvlct,r,g,b,/get
;
; set up postscript file
;
	orig_device = !d.name
	set_plot,'ps'
	xsize = 10.0
	ysize = 7.5
	device,/land,xsize=xsize,ysize=ysize,xoff=0.5,yoff=10.5,color=color, $
		file=file,bits=8,/inches
	!p.font = 0
;
; load color table 
;
	if color then tvlct,frebin(r,256),frebin(g,256),frebin(b,256)
;
; display images
;
	tv,pic1,0,0,xsize=10,ysize=1.25,/inches
	tv,pic2,7,4,xsize=3,ysize=3,/inches
	s = size(pic3) & nx = float(s(1)) & ny = float(s(2))
	rat = nx/ny
	if rat gt 745.0/515.0 then begin
		xsize3 = 6.94
		ysize3 = 6.94/rat
	   end else begin
	   	ysize3 = 4.8
		xsize3 = 4.8*rat
	end
	yoff3 = 7.0-ysize3
	tv,pic3,0,yoff3,xsize=xsize3,ysize=size3,/inches

;
; Display color bar
;
	bar = bindgen(256,2)
	if keyword_set(reversed) then bar = reverse(bar)
	tv,bar,1.25,1.45,xsize=4.25,ysize=0.3,/inches
;
; write image scaling information
;
	widget_control,info.min_field,get_value=imin
	widget_control,info.max_field,get_value=imax
	i = widget_info(info.scale_base,/droplist_select)
	titles = ['Linear Display','Square Root Display', $
		  'Logarithmic Display','Histogram Equalized Display']
	title = titles(i)
	xyouts,3.375/xsize,1.85/ysize,title,/norm,align=0.5
	xyouts,1.2/xsize,1.55/ysize,strtrim(imin,2),align=1.0,/norm
	xyouts,5.6/xsize,1.55/ysize,strtrim(imax,2),align=0.0,/norm
;
; draw location of pic2 in pic3
;
	x1 = info.xoffzoom
	y1 = info.yoffzoom
	x2 = x1 + 256/info.zoom_factor
	y2 = y1 + 256/info.zoom_factor
	if (x1 ge v(0)) and (y1 ge v(1)) and (x2 le (v(0)+nx-1)) $
	   and (y2 le (v(1)+ny-1)) then begin
	   	x1 = (x1 - v(0))/nx*xsize3/xsize
		x2 = (x2 - v(0))/nx*xsize3/xsize
		y1 = ((y1 - v(1))/ny*ysize3+yoff3)/ysize
		y2 = ((y2 - v(1))/ny*ysize3+yoff3)/ysize
		plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2
		plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2, $
			color=255,line=2
	endif
;
; draw location of pic3 in pic1
;
	x1 = v(0)/float(ns)
	x2 = (v(0) + nx - 1)/float(ns)
	y1 = v(1)/float(nl)*1.25/ysize
	y2 = (v(1) + ny - 1)/float(nl)*1.25/ysize
	plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2
	plots,[x1,x2,x2,x1,x1],[y1,y1,y2,y2,y1],/norm,thick=2,color=255,line=2
;
; plot boxes around images
;
	x = [0,1,1,0,0]
	y = [0,0,1,1,0]
	plots,x*10/xsize,y*1.25/ysize,/norm,thick=2
	plots,x*xsize3/xsize,(y*ysize3+yoff3)/ysize,/norm,thick=2
	plots,(x*3+7)/xsize,(y*3+4)/ysize,/norm,thick=2
	plots,(x*4.25+1.25)/xsize,(y*0.3+1.45)/ysize,/norm,thick=2
;
; write position information
;
	i = widget_info(info.display_base,/droplist_select)
	image_type = (['Raw X/Y','FARF','Final X/Y'])[i]
	
	widget_control,info.big_position,get_value=v
	xyouts,0.01,(yoff3-0.15)/ysize,v(0)+' '+image_type,/norm,charsize=0.8
	widget_control,info.zoom_position,get_value=v
	xyouts,7.1/xsize,3.85/ysize,v(0)+'  zoom = '+ $
			strtrim(info.zoom_factor,2),/norm,charsize=0.8
;
; write header information
;
	xpos = 7/xsize
	ypos = 3.7/ysize
	yoff = 0.2/ysize
	fdecomp,input.files(0),disk,dir,name,ext
	name = name + '.ext'
	if input.nfiles gt 1 then $
		name = name + '  ('+strtrim(input.nfiles,2)+' files)'
	xyouts,5.0/xsize,7.1/ysize,name,/norm,align=0.5
	header = *input.header(0)
	xyouts,xpos,ypos-yoff,/norm, $
		'Targname = '+strtrim(sxpar(header,'targname'),2)
	xyouts,xpos,ypos-yoff*2,/norm,strtrim(sxpar(header,'dateobs'),2) + $
			' '+ strtrim(sxpar(header,'timeobs'),2)
	xyouts,xpos,ypos-yoff*3,/norm, $
			'Detector = '+strtrim(sxpar(header,'detector'),2)
	xyouts,xpos,ypos-yoff*4,/norm, $
			'Aperture = '+strtrim(sxpar(header,'aperture'),2)
	device,/close
	set_plot,orig_device
	if color then tvlct,r,g,b
end

;=====================================================  CF_EDIT_EVENT_SELECTION
;
; Routine to select which events to process
;

pro cf_edit_event_selection, selection, group_leader=group_leader

	common cf_edit_selection_com, orig, new, info
	
	orig = selection
	new = selection

	main = widget_base(group_leader=group_leader,/modal,/column, $
		title='Selection Criteria of Events to Display/Extract')

	
	button_values = ['Good','Bad','Either']
	
	group_base = lonarr(8)
	group_titles = ['User Defined','Jitter','Not in OPUS GTI','Bursts', $
		'High Voltage','SAA','Limb angle','Day/Night']
	
	base = widget_base(main,/column,/frame)
	label = widget_label(base,value='Original IDF Time Flags')
	
	for i=0,6 do begin
		base1 = widget_base(base,/row)
		group_base(i) = cw_bgroup(base1,button_values, $
			set_value=orig.timeflags(i),uvalue='SELECT', $
			/row,/exclusive,/no_release)
		label = widget_label(base1,value=group_titles(i))
	end
	
	base1 = widget_base(base,/row)
	group_base(7) = cw_bgroup(base1,['Night','Day','Either'], $
		set_value=orig.timeflags(7),uvalue='SELECT', $
		/row,/exclusive,/no_release)
	label = widget_label(base1,value=group_titles(7))
	
	
	base = widget_base(main,/row,/frame)
	label = widget_label(base,value='PHA Range:    ')
	min_pha = cw_field(base,title='Min:',xsize=8,/row, $
			value=orig.pha_range[0],/long)
	max_pha = cw_field(base,title='Max:',xsize=8,/row, $
			value=orig.pha_range[1],/long)
	
;       base = widget_base(main,/row,/frame)
;       label = widget_label(base,value='Time Range (Seconds):    ')
;       min_time = cw_field(base,title='Min:',xsize=12,/row, $
;		value=orig.time_range[0],/float)
;       max_time = cw_field(base,title='Max:',xsize=12,/row, $
;		value=orig.time_range[1],/float)
	
	base = widget_base(main,/row,/frame)
	label = widget_label(base,value='Use my GTI selection ')
	my_gti = cw_bgroup(base,['No','Yes'], $
			set_value=orig.use_gti,uvalue='SELECT',/row,/exclusive)
	
	base = widget_base(main,/row)
	button = widget_button(base,value='       Done        ',uvalue='DONE')
	button = widget_button(base,value='       Abort       ',uvalue='ABORT')
	button = widget_button(base,value='       Reset       ',uvalue='RESET')
	button = widget_button(base,value='    Select All    ', $
			uvalue='SELECT ALL')	
	info = {group_base:group_base, $
;	        min_time:min_time, max_time:max_time, $
		min_pha:min_pha, max_pha:max_pha, my_gti:my_gti} 

	widget_control,main,/realize
	xmanager,'cf_edit_event_selection',main
	selection = new
end
;
; EVENT DRIVER
;
pro cf_edit_event_selection_event,event
	common cf_edit_selection_com, orig, new, info
;
; Get current values
;
	for i=0,7 do begin
		widget_control,info.group_base[i],get_value=v
		new.timeflags[i] = v
	end
	
	widget_control,info.my_gti,get_value=v
	new.use_gti = v
	
	widget_control,info.min_pha,get_value=v1
	widget_control,info.max_pha,get_value=v2
	new.pha_range = [v1,v2]
	
;	widget_control,info.min_time,get_value=v1
;	widget_control,info.max_time,get_value=v2
;	new.time_range = [v1,v2]
;
; process event
;

	widget_control,event.id,get_uvalue=uvalue
	case uvalue of
		'DONE': begin
			widget_control,event.top,/destroy
			return
			end
		'ABORT': begin
			new = orig
			widget_control,event.top,/destroy
			return
		        end
		'RESET': new = orig
		'SELECT ALL': begin
			new.timeflags = replicate(2,8)
			new.use_gti = 0
			new.pha_range = [0,31]
;			new.time_range = new.full_time_range
			end
		else:
	endcase
;
; set values
;
	for i=0,7 do $
		widget_control,info.group_base[i],set_value=new.timeflags[i]
	
	widget_control,info.my_gti,set_value=new.use_gti
	widget_control,info.min_pha,set_value=new.pha_range[0]
	widget_control,info.max_pha,set_value=new.pha_range[1]
;	widget_control,info.min_time,set_value=new.time_range[0]
;	widget_control,info.max_time,set_value=new.time_range[1]
end
;======================================================== CF_EDIT_GTI_SELECTION
;
; Routine to edit good time intervals from plot of selected parameter
;
pro cf_edit_gti_selection,info,input,iselect,selection,group_leader=group_leader


	tagname = (['','TIME_SUNRISE','TIME_SUNSET','LIMB_ANGLE','LONGITUDE', $
		'LATITUDE','ORBITAL_VEL','HIGH_VOLTAGE','LIF_CNT_RATE', $
		'SIC_CNT_RATE','FEC_CNT_RATE', 'AIC_CNT_RATE', $
		'BKGD_CNT_RATE','YCENT_LIF','YCENT_SIC'])[iselect]
;
; concatenate time vectors, current mask, and values of the
; selected tag
;
	n = input.nfiles
	ipos = lonarr(n)
	np = lonarr(n)
	npoints = 0
	
	for i=0,n-1 do begin
		n1 = n_elements((*input.timeline[i]).time)
		ipos(i) = npoints
		np(i) = n1
		npoints = npoints + n1
	end
	
	time = dblarr(npoints)
	mask = bytarr(npoints)
	values = fltarr(npoints)
	mask2 = bytarr(npoints)
	stflags = bytarr(npoints)
	
	for i=0,n-1 do begin
		time(ipos(i)) = (*input.timeline[i]).time + input.timestart[i]
		mask(ipos(i)) = *input.mask[i]
		stflags(ipos(i)) = (*input.timeline[i]).status_flags
		names = tag_names(*input.timeline[i])
		itag = where(names eq tagname,ngood) & itag = itag[0]
		if ngood ne 1 then begin
			istat = dialog_message(tagname + ' not found in the' +$
				'Timeline table for one of the files',/error, $
				dialog_parent = info.base)
			return
		end
		values(ipos(i)) = (*input.timeline[i]).(itag)
		cf_edit_mask2,input,i,selection,m

		mask2(ipos(i)) = m
	end
;
; edit good time interval mask
;
	line_edit2,time,values,mask,mask2,stflags=stflags,/modal,group=info.base, $
		xtitle='Time (seconds)',ytitle=tagname
;
; save results
;
	for i=0,n-1 do *input.mask[i] = mask(ipos(i):ipos(i)+np(i)-1)
		
end

;============================================================== CF_EDIT_SELECT
;
; Routine to select good events for ith file
;
pro cf_edit_select,input,i,selection,mask,channel=channel,exptime=exptime

	widget_control,/hourglass
;
; create a mask of the good time intervals
;
	cf_edit_mask,input,i,selection,mask
;
; add up the number of good second intervals
;
	exptime = total(mask)
        
;
; select events for valid time periods
;
	tabinv,(*input.timeline[i]).time,(*input.event[i]).time,index
	mask = mask(round(index))
;
; select events using PHA range
;
	if (selection.pha_range[0] gt 0) or $
	   (selection.pha_range[1] lt 31) then begin
	       mask = (((*input.event[i]).pha ge selection.pha_range[0]) and $
		       ((*input.event[i]).pha le selection.pha_range[1])) $
			and mask
	end
;
; optional extraction channel
;

	if (keyword_set(channel)) then begin
		mask = mask and ((*input.event[i]).channel eq channel)
	end
	widget_control,/hourglass
end

;================================================================ CF_EDIT_MASK
;
; Create a mask of good time intervals for the timeline of the ith
; obsevation
;
pro cf_edit_mask,input,i,selection,mask
;
; select valid time periods using status flags
;
	flag = (*input.timeline[i]).status_flags
	mask = replicate(1B,n_elements(flag))
	bitmasks = byte([128,64,32,16,8,4,2,1])
	for ibit = 0,7 do begin
	     bitmask = bitmasks(ibit)
	     case selection.timeflags(ibit) of
		0: mask = mask and ((flag and bitmask) eq 0)	;keep flag
		1: mask = mask and ((flag and bitmask) ne 0)	;reverse flag
		2: 						;ignore
	     endcase
	endfor
;
; select time periods using user specified GTI mask
;
	if selection.use_gti eq 1 then mask = mask and (*input.mask[i])
;
; select events using time range
;
;	if (selection.time_range(0) gt 0) or $
;	   (selection.time_range(1) lt selection.full_time_range(1)) then begin
;		time0 = selection.time_range(0) - input.timestart(i)
;		time1 = selection.time_range(1) - input.timestart(i)
;	   	mask = (((*input.timeline[i]).time ge time0) and $
;		        ((*input.timeline[i]).time le time1)) $
;			 and mask
;	end
end

;================================================================ CF_EDIT_MASK2
;
; Create a mask of good time intervals for the timeline of the ith
; obsevation
;
pro cf_edit_mask2,input,i,selection,mask
;
; select valid time periods using status flags
;
	flag = (*input.timeline[i]).status_flags
	mask = replicate(1B,n_elements(flag))
	bitmasks = byte([128,64,32,16,8,4,2,1])
	for ibit = 0,7 do begin
	     bitmask = bitmasks(ibit)
	     case selection.timeflags(ibit) of
		0: mask = mask and ((flag and bitmask) eq 0)	;keep flag
		1: mask = mask and ((flag and bitmask) ne 0)	;reverse flag
		2: 						;ignore
	     endcase
	endfor

end
; =========================================================  CF_EDIT_LINEPLOT
;
; Routine to plot row/column sums
;
pro cf_edit_lineplot,info,event,image,x,y,window
;

;
; If two input parameters supplied, initialize state and print instructions
;
;
	if n_params(0) lt 3 then begin
		states = ['','ROW','COL','RSUM','CSUM']
		info.cursor_state = 'PLOT' 
		info.cursor_substate = states(event.value) 
		case info.cursor_substate of
		    'ROW': mess = 'Select row and click left mouse button'
		    'COL': mess = 'Select column and click left mouse button'
		    'RSUM': mess = 'Select first row and click left button'
		    'CSUM': mess = 'Select first column and click left button'
		endcase
		info.xsave = -1
		info.x1 = -1
		widget_control,info.message,set_value=mess
		return
	end
;
; erase previous overlay
;
        x1 = info.xsave
        y1 = info.ysave
        ns = info.ns
        nl = info.nl
	if info.xsave ge 0 then begin 
	    case info.cursor_substate of 
		'ROW': cf_edit_plots,info,[0,ns],[y1,y1],/o,color=255
		'COL': cf_edit_plots,info,[x1,x1],[0,nl],/o,color=255
		'RSUM': cf_edit_plots,info,[0,ns],[y1,y1],/o,color=255
		'CSUM': cf_edit_plots,info,[x1,x1],[0,nl],/o,color=255
	    end
	end
	info.xsave = x
	info.ysave = y
;
;  plot new overlay (return if no button pressed)
;
	case info.cursor_substate of 
	    'ROW': cf_edit_plots,info,[0,ns],[y,y],/o,color=255
	    'COL': cf_edit_plots,info,[x,x],[0,nl],/o,color=255
	    'RSUM': cf_edit_plots,info,[0,ns],[y,y],/o,color=255
	    'CSUM': cf_edit_plots,info,[x,x],[0,nl],/o,color=255
	end
	if event.press eq 0 then return
;
; Row Plot
;
	if info.cursor_substate eq 'ROW' then begin
		cf_edit_plots,info,[0,ns],[y,y],/o,color=255
		cf_edit_plots,info,[0,ns],[y,y],line=1
		xrange = [0,ns]	
		if window eq 'ZOOM' then $
			xrange = [info.xoffzoom,info.xoffzoom+ $
						256/info.zoom_factor]
		if window eq 'BIG' then begin
			widget_control,info.big_window,get_draw_view=v
			xrange = [v(0),(v(0)+720)<(ns-1)]
		end
		cf_edit_xyttag,info,xrange,0,/frac
		xv = findgen(ns)
		cf_edit_xyttag,info,xv,0,/frac
		lineplot,xv,image(*,y),title='  Row '+ $
			strtrim(y*info.ybin,2),xrange=xrange
	end
;
; Column Plot
;	
	if info.cursor_substate eq 'COL' then begin
		cf_edit_plots,info,[x,x],[0,nl],/o,color=255
		cf_edit_plots,info,[x,x],[0,nl],line=1	
		xrange = [0,nl]
		if window eq 'ZOOM' then $
			xrange = [info.yoffzoom,info.yoffzoom+ $
					256/info.zoom_factor]
		if window eq 'BIG' then begin
			widget_control,info.big_window,get_draw_view=v
			xrange = [v(1),(v(1)+512)<(nl-1)]
		end
		cf_edit_xyttag,info,0,xrange,/frac
		xv = findgen(nl)
		cf_edit_xyttag,info,0,xv,/frac
		lineplot,xv,reform(image(x,*)),title='  Column '+ $
				strtrim(x*info.xbin,2),xrange=xrange
	end
;
; Row Sum
;
	if info.cursor_substate eq 'RSUM' then begin
	    if info.x1 ne -1 then begin
	        info.xsave = -1
	        cf_edit_plots,info,[0,ns],[y,y],/o,color=255
		cf_edit_plots,info,[0,ns],[y,y],line=1
		xrange = [0,ns]
		if window eq 'ZOOM' then $
		     xrange = [info.xoffzoom,info.xoffzoom+256/info.zoom_factor]
		if window eq 'BIG' then begin
			widget_control,info.big_window,get_draw_view=v
			xrange = [v(0),(v(0)+720)<(ns-1)]
		end
		y1 = info.y1<y
		y2 = info.y1>y
		if y1 eq y2 then data = image(*,y1) $
			    else data = total(image(*,y1:y2),2)
		title = 'Rows '+strtrim(y1*info.ybin,2)+' to '+ $
				strtrim(y2*info.ybin,2)
		cf_edit_xyttag,info,xrange,0,/frac
		xv = findgen(ns)
		cf_edit_xyttag,info,xv,0,/frac

		lineplot,xv,data,title=title,xrange=xrange 
	    end else cf_edit_plots,info,[0,ns],[y,y],line=1
	end
;
; Column Sum
;
	if info.cursor_substate eq 'CSUM' then begin
	    if info.x1 ne -1 then begin	
	        cf_edit_plots,info,[x,x],[0,nl],/o,color=255
		cf_edit_plots,info,[x,x],[0,nl],line=1
	        info.xsave = -1
		xrange = [0,nl]
		if window eq 'ZOOM' then $
			xrange = [info.yoffzoom,info.yoffzoom+ $
				256/info.zoom_factor]
		if window eq 'BIG' then begin
			widget_control,info.big_window,get_draw_view=v
			xrange = [v(1),(v(1)+512)<(nl-1)]
		end
		x1 = info.x1<x
		x2 = info.x1>x
		if x1 eq x2 then data = reform(image(x1,*)) $
			    else data = total(image(x1:x2,*),1)
		title = 'Columns '+strtrim(x1*info.xbin,2)+' to '+ $
				   strtrim(x2*info.xbin,2)
		cf_edit_xyttag,info,xrange,0,/frac
		xv = findgen(nl)
		cf_edit_xyttag,info,xv,0,/frac
		lineplot,xv,data,title=title,xrange=xrange
	    end else cf_edit_plots,info,[x,x],[0,nl],line=1
	end
;
; Do we need second point?
;
	if ((info.cursor_substate eq 'RSUM') or $
	   (info.cursor_substate eq 'CSUM')) and $
	   (info.x1 eq -1) then begin
		if info.cursor_substate eq 'RSUM'  $
			then mess = 'Select last row and click left button' $
			else mess = 'Select last column and click left button'
	    	widget_control,info.message,set_value = mess
		info.x1 = x
		info.y1 = y
	    end else begin
	    	widget_control,info.message,set_value = ' '
		info.cursor_state = 'NONE'
		info.cursor_substate = ''
		info.x1 = -1
		info.xsave = -1
	end
	return
end

;============================================================ CF_EDIT_PLOT_PHA
;
; Routine to plot histogram of the PHA values
;
pro cf_edit_plot_pha,info,input

	phist = lonarr(32)
	for i=0,input.nfiles-1 do phist = histogram( (*input.event(i)).pha, $
					min=0,max=31,input=phist)
	lineplot,indgen(32),phist,xtitle='Pulse Height', $
			ytitle='Number of Occurences'
end

;=========================================================== CF_EDIT_EXTRACT
;
; Routine to extract a spectrum
;
pro cf_edit_extract,info,menu_number,input,selection
;
	channel = ([0,1,2,3,5,6,7])[menu_number]
	widget_control,/hourglass
	title = (['','LiF HIRS','LiF MDRS','LiF LWRS','LiF PINH', $
		     'SiC HIRS','SiC MDRS','SiC LWRS','SiC PINH'])[channel]
;
; determine wavelengths
;
        
;	w = (*input.event[0]).lambda
;       good = where(w GT 0)
;       w = w(good)
;	wmin = min(w,max=wmax)
;	wmin = fix(wmin)
;	wmax = ceil(wmax)
        
        detector = strtrim(sxpar((*input.header(0)),'detector'),2)
        
        case detector of
           '1A': begin
              wmin = [985,997]
              wmax = [1085,1097]
           end
           '1B': begin
              wmin = [1090,900]
              wmax = [1190,1000]
           end
           '2A': begin
              wmin = [1085,910]
              wmax = [1185,1010]
           end
           '2B': begin
              wmin = [977,1010]
              wmax = [1077,1110]
           end
        endcase
        
        IF (channel LE 4) THEN BEGIN
           wmin = wmin[0]
           wmax = wmax[0]
        ENDIF ELSE BEGIN
           wmin = wmin[1]
           wmax = wmax[1]
        ENDELSE
        
        
        dw = info.deltaw > 0.005
	n = round((wmax-wmin)/dw)
        wave = dindgen(n)*dw+wmin
        flux = fltarr(n)
;
; loop on files
;	
	totexpo = 0.0
	for i=0,input.nfiles-1 do begin

;
; select events using time flags and GTI mask
;
	    cf_edit_select,input,i,selection,mask,channel=channel, $
	    		exptime=exptime
	    totexpo = totexpo + exptime
	    good = where(mask,ngood)
	    if ngood eq 0 then goto,nexti
	    w = (*input.event[i]).lambda[good]
	    bin = round((w-wmin)/dw)
	    if info.flux eq 0 then begin
	    	flux = histogram(bin,min=0,max=n-1,input=flux)
	      end else begin
		cf_edit_drizzle,flux,bin,(*input.event[i]).ergcm2[good]
	    end
nexti:
	end

	if info.flux eq 0 then begin
	    ytitle = 'Counts'
	  end else begin
	    flux = flux/(totexpo>1)
	    ytitle = 'Flux'
	    flux = flux/dw
	end
	
        mess = 'WARNING: Not Background Subtracted'
        
        lineplot,wave,flux, $
	title=title,xtitle='Wavelength (A)',ytitle='Flux (cgs)', ptitle= mess

	widget_control,/hourglass
end
;============================================================= CF_EDIT_DRIZZLE
; 
pro cf_edit_drizzle, vector, index, values
;    Add values to an array at specified indicies.     The basic usage is
;       FDRIZZLE, vector, index, values 
;    where INDEX and VALUES should have same number of elements.   If there are
;       no duplicates in INDEX then FDRIZZLE simply performs the assignment
;    VECTOR[INDEX] = VECTOR[INDEX] + VALUES 
;       But if INDEX contains repeated elements then the corresponding VALUES
;       will be summed together.
;
;    Use the REVERSE_ELEMENTS keyword of histogram to determine the repeated
;    values in INDEX and vector sums these together.

        h = histogram(index,reverse = rindex,min=0,max=N_elements(vector)-1)
;
; Add locations with at least one pixel
;
        gmax = max(h)         ;Highest number of duplicate indicies
        for i=1,gmax do begin
            g = where(h GE i, Ng) 
            if Ng GT 0 then begin 
               vector[g] = vector[g] + values[rindex[ rindex[g]+i-1]]
            endif
        endfor
end
;=============================================================== CF_EDIT_PLOTS
;
; Routine to draw horizontal / vertical overlay lines
;
pro cf_edit_plots,info,xx,yy,color=color,overlay=overlay,linestyle=linestyle

;
; plot vector in all three windows
;
;
; convert to window coordinates for all three windows and plot
;
	if n_elements(color) eq 0 then color = !d.n_colors-1
	if keyword_set(overlay) then color=!d.n_colors-1
	if n_elements(linestyle) eq 0 then linestyle=0

	wset,info.big_id
	if keyword_set(overlay) then device,set_graphic=6
	plots,xx,yy,/dev,color=color,line=linestyle

	wset,info.little_id
	if keyword_set(overlay) then device,set_graphic=6
	factorx = 1024./info.ns
	factory = 128.0/info.nl
	plots,long(xx*factorx),long(yy*factory),/dev,color=color,line=linestyle

	wset,info.zoom_id
	if keyword_set(overlay) then device,set_graphic=6
	xpos = (xx-info.xoffzoom)*long(info.zoom_factor) + info.zoom_factor/2
	ypos = (yy-info.yoffzoom)*long(info.zoom_factor) + info.zoom_factor/2
	plots,xpos>(-32768)<32767,ypos>(-32768)<32767, /dev, $
	      		color=color,line=linestyle
	device,set_graphic=3
return
end
;====================================================== CF_EDIT_SPECIFY_OVERLAY
;
; Routine to specify what overlay elements are to be displayed
;
pro cf_edit_specify_overlay,info
	
	main = widget_base(group=info.base,/modal,/column, $)
		title='Graphic Overlays')
	
	base = widget_base(main,/col,/frame)

	button_base = cw_bgroup(base,/nonexclusive,['Wavelength Scales', $
		'Background Regions','HIRS Extraction Region', $
		'MDRS Extraction Region','LWRS Extraction Region'], $
		 uvalue='BGROUP',/column)
	button = widget_button(main,value='EXIT',uvalue='EXIT')
	ptr = ptr_new({overlay:info.overlay,button_base:button_base})
	widget_control,main,/realize
	widget_control,button_base,set_value=info.overlay
	widget_control,main,set_uvalue=ptr
	xmanager,'cf_edit_specify_overlay',main
	info.overlay = (*ptr).overlay
	ptr_free,ptr
end
pro cf_edit_specify_overlay_event,event
	widget_control,event.top,get_uvalue=ptr
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'EXIT' then begin
		widget_control,(*ptr).button_base,get_value=v
		(*ptr).overlay = v
		widget_control,event.top,/destroy
	end
end	
;===================================================== CF_EDIT_DISPLAY_OVERLAY
; Routine to plot wavelength overlay
;
pro cf_edit_display_overlay,info,input

   	if info.overlay(0) eq 0 then goto,overlay_background
	detector = strtrim(sxpar((*input.header(0)),'detector'),2)
;
; overlay wavelength scales --------------------------------------
;
;
; get coefficients
;
	case detector of
	  '1A': begin
	  	coef = [[-135022.83,       127.97941D0,    0.0099817230D0], $
	   		[175071.24,      -156.89625D0,   -0.0024052021D0]]
                wmin = [985,1000]
                wmax = [1085,1095]
		end
	  '1B': begin
	  	coef = [[-139906.61,       109.03151D0,     0.018138421D0], $
	   		[161618.17,      -162.80883D0,    0.0011034282D0]]
                wmin = [1090,900]
                wmax = [1190,995]
		end
	  '2A': begin
	  	coef = [[167070.27,      -130.72053D0,   -0.0082661123D0], $
	   		[-139746.96,       147.48503D0,    0.0066074294D0]]
                wmin = [1085,915]
                wmax = [1185,1010]
		end
	  '2B': begin
	  	coef = [[166445.98,      -159.89416D0,    0.0057074141D0], $
	   		[-136294.76,       110.58599D0,     0.024242810D0]]
                wmin = [977,1015]
                wmax = [1077,1105]
		end
	  else: goto,overlay_background
	endcase
;
; plot on big window
;
	wset,info. big_id
	!fancy = 1
	ypos = [1024/info.ybin-25,25]	;y-position of grid
	ydir = [-1,1]			;direction of the ticks
	for i=0,1 do begin		;loop on the 2 channels
		w = [wmin(i),wmax(i)]
		c = coef(*,i)
		x = c(0) + c(1)*w + c(2)*w*w
		x = x/info.xbin
		y = ypos(i)
		plots,x,[y,y],/dev,thick=2
		for ww=w(0),w(1),5 do begin
			x = (c(0) + c(1)*ww + c(2)*ww*ww)/info.xbin
			plots,[x,x],[0,ydir(i)*10]+y,/dev,thick=2
			if ydir(i) eq -1 then yoff=5 else yoff=-15
			xyouts,x,y+yoff,strtrim(ww,2),align=0.5,/dev, $
					charsize=1.5
		end
		for ww=w(0),w(1) do begin
			x = (c(0) + c(1)*ww + c(2)*ww*ww)/info.xbin
			plots,[x,x],[0,ydir(i)*6]+y,/dev,thick=2
		end
	end
;
; plot on little window
;
	wset,info.little_id
	ypos = [124,4]
	for i=0,1 do begin		;loop on the 2 channels
		w = [wmin(i),wmax(i)]
		c = coef(*,i)
		x = c(0) + c(1)*w + c(2)*w*w
		x = x/16
		y = ypos(i)
		plots,x,[y,y],/dev,thick=2
		if ydir(i) eq -1 then yoff=-15 else yoff=8
		xyouts,x(0),y+yoff,strtrim(w(0),2),align=0.5,/dev
		xyouts,x(1),y+yoff,strtrim(w(1),2),align=0.5,/dev
		for ww=w(0),w(1),10 do begin
			x = (c(0) + c(1)*ww + c(2)*ww*ww)/16
			plots,[x,x],[0,ydir(i)*5]+y,/dev,thick=2
		end
	end
	xyouts,10,30,'SiC '+detector,/dev,charsize=1.5
	xyouts,10,80,'LiF '+detector,/dev,charsize=1.5
overlay_background:
;
; overlay background regions ---------------------------------------
;
	if info.overlay(1) eq 0 then goto,overlay_extraction
	for i=0,7 do begin
		
		if info.bpos[i*2+1] gt 0 then begin
			xpos = [500,16000]
			ypos = [info.bpos[i*2],info.bpos[i*2+1]]
			cf_edit_xyttag,info,xpos,ypos,/to_binned
			wset,info.big_id
			plots,xpos([0,1,1,0,0]),ypos([0,0,1,1,0]),/dev
			wset,info.little_id
			cf_edit_convert,info,xpos,ypos,'little',xout,yout, $
				/to_screen
			plots,xout([0,1,1,0,0]),yout([0,0,1,1,0]),/dev
		end

	end					

;
; overlay extraction windows ----------------------------------------
;

overlay_extraction:

	if max(info.overlay(2:4)) eq 0 then goto,done
	cf_edit_get_ext_regions,info,input
	if input.chid_cal eq '' then begin
		info.overlay(2:4) = 0
		goto,done
	end
	slitname=["HIRS","MDRS","LWRS","PINH","HIRS","MDRS","LWRS","PINH"]
	specname=["LiF","LiF","LiF","LiF","SiC","SiC","SiC","SiC"]    
	for i=0,2 do begin				;loop on apertures
	    if info.overlay(i+2) ne 0 then begin
		for j=0,1 do begin			;loop on channels
		    k = j*4+i
		    apertext = slitname(k)+' '+specname(k)
	    	    ylow = *input.ylow(k)
		    yhigh = *input.yhigh(k)
		    deltay = nint((ylow[0]+yhigh[0])/2. - info.ycents(k+1))
		    yhigh = yhigh - deltay
		    ylow = ylow - deltay
		    x = indgen(16384)
		    cf_edit_xyttag,info,x,ylow,/to_binned
		    cf_edit_xyttag,info,0,yhigh,/to_binned
		    wset,info.big_id
		    cf_edit_convert,info,x,ylow,'BIG',xout,yout1,/to_screen
		    cf_edit_convert,info,x,yhigh,'BIG',xout,yout2,/to_screen
		    plots,xout,yout1,/device
		    plots,xout,yout2,/device
		    xyouts,xout(15500),yout1(15500)+3,apertext,/dev,charsize=1.5
		
		    wset,info.little_id
		    cf_edit_convert,info,x,ylow,'LITTLE',xout,yout1,/to_screen
		    cf_edit_convert,info,x,yhigh,'LITTLE',xout,yout2,/to_screen
		    plots,xout,yout1,/device
		    plots,xout,yout2,/device
		    xyouts,xout(15500),yout1(15500)+2,apertext,/dev
		end
	    end
	end
done:
return
end
;====================================================== CF_EDIT_GET_EXT_REGIONS
;
; Routine to read extract region reference file
;
;
pro  cf_edit_get_ext_regions,info,input
;
; Check if already read.
;
	if input.chid_cal ne '' then return	
;
; get file name
;
	chid_cal = sxpar((*input.header(0)),'CHID_CAL')
	if !err lt 0 then begin
		Message = 'No CHID_CAL keyword found in header'
		goto,abort
	end
	chid_cal = strtrim(chid_cal,2)
	filename = file_search(info.caldir+'/'+chid_cal) & filename=filename(0)
        
	if filename eq '' then begin
		Message = chid_cal + ' not found in CF_CALDIR' 
		goto,abort
	end
;
; get extraction type
;
	src_type = strtrim(sxpar((*input.header(0)),'src_type'))
	if strmid(src_type,0,1) eq 'E' then ioff=9 else ioff=1

	for i=0,7 do begin
		iextend = i+ioff
		if (iextend mod 4) ne 0 then begin
			a = mrdfits(filename,iextend,chid_cal_header,/silent,/fscale)
			input.ylow(i) = ptr_new(a.ylow)
			input.yhigh(i) = ptr_new(a.yhigh)
                        info.ycents_default(i) = sxpar(chid_cal_header,'CENTROID')
		end
	end
	input.chid_cal = chid_cal
	return
abort:
	istat = dialog_message(message,/error,dialog_parent=info.base)
	return
end


;=========================================================== CF_EDIT_BACKGROUND
;
; Routine to edit background regions
;
;
pro cf_edit_background,info,input,event,x,y,window,image,little_image
;
; If two input parameters supplied, initialize state and print instructions
;
	if n_params(0) lt 4 then begin
		info.cursor_state = 'BACKGROUND'
		states = ['ADJUST','NEW','REMOVE']
		info.cursor_substate = states(event.value-1) 
		case info.cursor_substate of
		    'ADJUST': mess = 'Select Background Position and ' + $
		    			'click mouse button'
		    'NEW': mess = 'Select Beginning of Region and' + $
		    			'click mouse button'
		    'REMOVE': mess = 'Place Cursor in Region and ' + $
		    			'click mouse button'		    
		endcase
		info.ysave = -1
		info.y1 = -1
		widget_control,info.message,set_value=mess
		return
	end

;
; erase previous overlay
;
	if (info.cursor_substate ne 'NEW') and (info.ysave eq -1) and $
	   (event.press eq 0) then return
        x1 = info.xsave
        y1 = info.ysave
        ns = info.ns
        nl = info.nl
	if info.ysave ge 0 then cf_edit_plots,info,[0,ns],[y1,y1],/o,color=255
	info.xsave = x
	info.ysave = y
;
;  plot new overlay (return if no button pressed)
;
	cf_edit_plots,info,[0,ns],[y,y],/o,color=255
	if event.press eq 0 then return
;
; ---------------  Adjust existing background region
;
	if info.cursor_substate eq 'ADJUST' then begin
	    if info.nback eq 0 then begin
	    	cursor.state = 'NONE'
		return
	    end
;
; find region to adjust?
;
	    if info.y1 eq -1 then begin
		ypos = y
		cf_edit_xyttag,info,0,ypos
		bpos = info.bpos(0:info.nback*2-1)
		diff = abs(bpos-ypos)
		good = where(diff eq min(diff)) & good = good(0)

		info.back_edit = good
		info.y1 = y
;
; blacken old position	
;
		xpos = [500,16000]
		yold = [bpos(good),bpos(good)]
		cf_edit_xyttag,info,xpos,yold,/to_binned
		wset,info.big_id
		plots,xpos,yold,/dev,color=0
		wset,info.little_id
		cf_edit_convert,info,xpos,yold,'little',xout,yout, $
			/to_screen
		plots,xout,yold,/dev,color=0
	      end else begin
;
; Adjust position
;
		ypos = y
		cf_edit_xyttag,info,0,ypos
		if info.bpos(info.back_edit+1) gt 0 then $
				ypos = ypos<(info.bpos(info.back_edit+1)-1)
		if info.back_edit gt 0 then $
				ypos = ypos>(info.bpos(info.back_edit-1)+1)
		info.bpos(info.back_edit) = ypos
		info.ysave = -1
		info.cursor_state = 'NONE'
		cf_edit_display,info,input,image,little_image
		return
	    end
	end
;
; --------------- Create New Region
;
	if info.cursor_substate eq 'NEW' then begin
	    if info.y1 eq -1 then begin
		cf_edit_plots,info,[0,ns],[y,y],line=1
		mess = 'Select last row and click left button'
	    	widget_control,info.message,set_value = mess
		info.y1 = y
	      end else begin
		cf_edit_plots,info,[0,ns],[y,y],line=1
		ypos = [info.y1<y,info.y1>y]
		cf_edit_xyttag,info,0,ypos
		nback = info.nback
		if nback eq 0 then begin
			bpos = ypos
		    end else begin
		    	bpos = info.bpos(0:nback*2-1)
			bpos = reform([bpos,ypos],2,nback+1)
		end
		nback = nback + 1
check:
		if nback gt 1 then begin
		    sub = sort(bpos(0,*))
		    bpos = bpos(*,sub)
		    for i=1,nback-1 do begin
			if bpos(0,i) le bpos(1,i-1) then $
						bpos(0,i) = bpos(1,i-1)+1
			if bpos(0,i) gt bpos(1,i) then begin
				bpos(0,i) = -1
				bpos(1,i) = bpos(1,i-1)
			end
		    end
		    good = where(bpos(0,*) ge 0,nback)
		    bpos = bpos(*,good)
		end
		for i=0,nback*2-1 do info.bpos(i) = bpos(i)
		info.nback = nback
		info.ysave = -1
		info.cursor_state = 'NONE'
		cf_edit_display,info,input,image,little_image
	    end
	end				
;
; -------------- Remove Region
;
	if info.cursor_substate eq 'REMOVE' then begin
	    nback = info.nback
	    if nback eq 0 then return
	    bpos = reform(info.bpos(0:nback*2-1),2,nback)
	    cf_edit_xyttag,info,0,y
	    for i=0,nback-1 do $
	    	if (y ge bpos(0,i)) and (y le bpos(1,i)) then bpos(0,i) = -1
	    good = where(bpos(0,*) ge 0,nback)
	    if nback gt 0 then bpos = bpos(*,good)
	    info.bpos = info.bpos*0
	    if nback gt 0 then for i=0,nback*2-1 do info.bpos(i) = bpos(i)
	    info.cursor_state = 'NONE'
	    cf_edit_display,info,input,image,little_image
	end
end
;================================================================ CF_EDIT_HEADER
;
; Routine to process events when header button is pressed
;
pro cf_edit_header,event,input

;
; if event.value is 1, display selected header
;
	if event.value eq 1 then begin
	
		files = strarr(input.nfiles)
		for i=0,input.nfiles-1 do begin
			fdecomp,input.files(i),disk,dir,name
			files(i) = name
		end
		
	    	if input.nfiles gt 1 then begin
			select_w,files,iselect,"","Select File",1,count=count, $
					group=event.top
			if count eq 0 then return
		end else iselect = 0
		
		file = files(iselect(0))
		xdisplayfile, "", group=event.top, font='6X13', $
			title=file, text=(*input.header(iselect(0)))
	end
;
; if event.value is 2 then add comments
;
	if event.value eq 2 then begin
		cf_edit_get_comments,comments,group=event.top
;		h = *input.header(0)
		sxaddhist,comments,*input.header(0)
;		*input.header(0) = h
	end
end

pro cf_edit_get_comments,comments,group_leader=group_leader

	main = widget_base(group=group_leader,/column,/modal)
	label = widget_label(main,value='Enter Header Comments into text box')
	text_base = widget_text(main,xsize=68,ysize=20,uvalue='MESSAGE',/edit)
	button = widget_button(main,uvalue='DONE',value='DONE')
	ptr = ptr_new({text_base:text_base,comments:ptr_new(strarr(1))})
	widget_control,main,/realize
	widget_control,main,set_uvalue=ptr
	xmanager,'cf_edit_get_comments',main
	comments = *(*ptr).comments
	ptr_free,ptr
end	
pro cf_edit_get_comments_event,event
	widget_control,event.top,get_uvalue=ptr
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'DONE' then begin
		widget_control,(*ptr).text_base,get_value=v
		(*(*ptr).comments) = v
		widget_control,event.top,/destroy
	end
end

;================================================================ CF_EDIT_WRITE
;
; Routine to write editted time-tag files
;
pro cf_edit_write,info,input,selection
;
;
; Get output filename
;
try_again:
	fdecomp,input.files(0),disk,dir,name
	if strlen(name) ne 22 then name ='output_idf.fit' else $
		name = strmid(name,0,8)+'cfe'+strmid(name,11,11)+'.fit'
	file = dialog_pickfile(file=name,filter='*.fit',/write, $
			dialog_parent=info.base, $
			title='Select Output IDF filename')
	if file eq '' then return	;no file selected
	list = file_search(file,count=count)
	if count gt 0 then begin
		result = dialog_message(file + ' already exists, ' + $
			'overwrite it?', dialog_parent=info.base,/question)
		if result ne 'Yes' then goto,try_again
	end
	
;
; create structures to hold output events and timeline table
;

	widget_control,/hourglass
	nfiles = input.nfiles
	n_in = lonarr(nfiles)
	ntime_in = lonarr(nfiles)
	nout = 0L
	ntime = 0L
        out_nevents = 0L

	for i=0,nfiles-1 do BEGIN
                out_nevents = out_nevents + sxpar(*input.header(i), "nevents")
		n_in(i) = n_elements((*input.event(i)).channel)
		ntime_in(i) = n_elements((*input.timeline(i)).time)
		nout = nout + n_in(i)
		ntime = ntime + ntime_in(i)
	end

	events = {	time:fltarr(nout,/nozero), $
			xraw:intarr(nout,/nozero), $
			yraw:intarr(nout,/nozero), $
			pha:bytarr(nout,/nozero), $
			weight:fltarr(nout,/nozero), $
			xfarf:intarr(nout,/nozero), $
			yfarf:intarr(nout,/nozero), $
			x:intarr(nout,/nozero), $
			y:intarr(nout,/nozero), $
			channel:bytarr(nout,/nozero), $
			timeflgs:bytarr(nout,/nozero), $
			loc_flgs:bytarr(nout,/nozero), $
			lambda:fltarr(nout,/nozero), $
			ergcm:fltarr(nout,/nozero)}

	timeline = {	time:fltarr(ntime,/nozero), $
			status_flags:bytarr(ntime,/nozero), $
			time_sunrise:intarr(ntime,/nozero), $
			time_sunset:intarr(ntime,/nozero), $
			limb_angle:intarr(ntime,/nozero), $
			longitude:intarr(ntime,/nozero), $
			latitude:intarr(ntime,/nozero), $
			orbital_vel:intarr(ntime,/nozero), $
			high_voltage:intarr(ntime,/nozero), $
			lif_cnt_rate:intarr(ntime,/nozero), $
			sic_cnt_rate:intarr(ntime,/nozero), $
			fec_cnt_rate:intarr(ntime,/nozero), $
			aic_cnt_rate:intarr(ntime,/nozero), $
			bkgd_cnt_rate:intarr(ntime,/nozero), $
			ycent_lif:intarr(ntime,/nozero), $
			ycent_sic:intarr(ntime,/nozero)}
;
; copy and scale event table
;
	tnames = ['XFARF','YFARF','X','Y']
	tscale = [0.25,0.1,0.25,0.1]
	tzero = [8192.,0.,8192.,0.]
	tagnames = tag_names(events)
	j1 = 0
	for i=0,nfiles-1 do begin
	    j2 = j1 + n_in(i)-1
	    for k=0,13 do begin
	        good = (where(tnames eq tagnames(k),ngood))[0]
		if ngood gt 0 then events.(k)[j1:j2] = round( $
			((*input.event[i]).(k) - tzero(good))/tscale(good)) $
		    else events.(k)[j1:j2] = (*input.event[i]).(k)
	    end
	    j1 = j2+1
	end
	event_header = (*input.event_header(0))
	for i=0,n_elements(tnames)-1 do begin
		good = (where(tagnames eq tnames[i]))[0]
		st = strtrim(good+1,2)
		sxaddpar,event_header,'TZERO'+st,tzero(i),' '
		sxaddpar,event_header,'TSCAL'+st,tscale(i),' '
	end
;
; copy and scale timeline table
;
	tnames = ['TIME_SUNRISE','TIME_SUNSET','LIMB_ANGLE','LONGITUDE', $
		  'LATITUDE','ORBITAL_VEL','YCENT_LIF','YCENT_SIC']
	tscale = [0.1,0.1,0.1,0.1,0.1,0.01,0.1,0.1]
	tzero = [3000.0,3000.0,0,0,0,0,0,0]	
	tagnames = tag_names(timeline)
	j1 = 0
	for i=0,nfiles-1 do begin
	    j2 = j1 + ntime_in(i)-1
	    for k=0,15 do begin
	        good = (where(tnames eq tagnames(k),ngood))[0]
		if ngood gt 0 then timeline.(k)[j1:j2] = round( $
				((*input.timeline[i]).(k) - $
				   tzero(good))/tscale(good)) $
		    else timeline.(k)[j1:j2] = (*input.timeline[i]).(k)
	    end
	    j1 = j2+1
	end
	timeline_header = (*input.timeline_header(0))
	for i=0,n_elements(tnames)-1 do begin
		good = (where(tagnames eq tnames[i]))[0]
		st = strtrim(good+1,2)
		sxaddpar,timeline_header,'TZERO'+st,tzero(i),' '
		sxaddpar,timeline_header,'TSCAL'+st,tscale(i),' '
	end
;
; get output header
;
	header = (*input.header(0))

	sxaddpar,header,'nevents',out_nevents
        fdecomp,file,disk,dir,name,ext
        sxaddpar,header,'filename',name+'.'+ext
        rootname = sxpar(header,'rootname')
        sxaddpar,  header, 'rootname',strmid(rootname,0, 8)+'999'


	nfiles = input.nfiles
	sxaddpar,header,'nspec',nfiles
	for i=0,nfiles-1 do begin
		fdecomp,input.files(i),disk,dir,name,ext
		sxaddpar,header,'spec'+string(i+1,'(I3.3)'),name+'.'+ext
	end
	for i=0,nfiles-1 do sxaddpar,header,'woffl'+string(i+1,'(I3.3)'), $
			input.woffset_lif(i)
	for i=0,nfiles-1 do sxaddpar,header,'woffs'+string(i+1,'(I3.3)'), $
			input.woffset_sic(i)



; set EXP_STAT to 0
	sxaddpar,  header, 'exp_stat',0		


;
; edit status flags
;
	flags = timeline.status_flags
;
; edit background regions
;
	sxaddpar,header,'bkgd_num',info.nback
	for i=0,info.nback-1 do begin
		sxaddpar,header,'bkg_min'+strtrim(i,2),info.bpos(i*2)
		sxaddpar,header,'bkg_max'+strtrim(i,2),info.bpos(i*2+1)
	end
;
; Reset bits 2 to 8 using the user specified selection criteria -----------
;
;
	bitmasks = byte([128,64,32,16,8,4,2])
	titles = ['User Defined','Jitter','Not in OPUS GTI','Bursts', $
		'High Voltage','SAA','Limb angle']
	for ibit = 0,6 do begin
		mask = bitmasks(ibit)
		case selection.timeflags(ibit) of
		   0: begin 				;keep flag
		      mess =''
		      end
		   1: begin				;reverse flag
		      bit = flags and mask
		      reverse_bit = (not bit) and mask
		      flags = reverse_bit or (flags and not mask)
		      mess = 'flags reversed'
		      end
		   2: begin
		      flags = (flags and not mask)	;ingore flag
		      mess = 'flags ignored'
		      end
		end
               if mess ne '' then sxaddhist,titles(ibit)+' '+mess,header
	end
;	if mess ne '' then sxaddhist,titles+' '+mess,header

;
; set daynight flag keyword
;
	values = ['NIGHT','DAY','BOTH']
	sxaddpar,header,'DAYNIGHT',values(selection.timeflags(7))
;
; Remove any user selected bad time intervals
;
	if selection.use_gti then begin
	    j1 = 0
	    for i=0,nfiles-1 do begin
		j2 = j1 + ntime_in(i)-1
		flags(j1:j2) = flags(j1:j2) or $
				(128B*((*input.mask(i)) eq 0))
		j1 = j2+1
	    end
	end
	timeline.status_flags = flags	
;
; Convert times to time since the start of the first observation
;
	j1 = n_in(0)		;pointer in event table
	k1 = ntime_in(0)  	;pointer in timeline table
	for i=1,nfiles-1 do begin
		j2 = j1 + n_in(i)-1
		k2 = k1 + ntime_in(i)-1
		events.time(j1:j2) = events.time(j1:j2) + input.timestart(i)
		timeline.time(k1:k2) = timeline.time(k1:k2) + input.timestart(i)
		j1 = j2+1
		k1 = k2+1
	endfor
;
; create mask of good time intervals
;
	case selection.timeflags(7) of 
		0: mask = (flags eq 0)					;night
		1: mask = ((flags and 254B) eq 0) and $
				    ((flags and 1B) eq 1) 		;day
		2: mask = ((flags and 254B) eq 0) 			;both
	endcase
	sxaddpar,header,'exptime',total(mask)
        sxaddpar,header,'expnight',total(flags EQ 0)
;
; select events using time range
;
;	if (selection.time_range(0) gt 0) or $
;	   (selection.time_range(1) lt selection.full_time_range(1)) then begin
;		time0 = selection.time_range(0) 
;		time1 = selection.time_range(1) 
;	   	mask = ((timeline.time ge time0) and $
;		        (timeline.time le time1)) $
;			 and mask
;	end
;
; create flag for each event
;
	tabinv,timeline.time,events.time,index
	events.timeflgs = flags(round(index))

;
; process PHA flagging
;
	sxaddpar,header,'phalow',selection.pha_range(0)
	sxaddpar,header,'phahigh',selection.pha_range(1)

        eflags=events.loc_flgs

	
	pha = events.pha
        
	bad = (pha lt selection.pha_range(0)) or $
	      (pha gt selection.pha_range(1))
	eflags = (eflags and 239B) or bad*16B
	

	events.loc_flgs = eflags
;
; create list of good time intervals
;

	n = n_elements(timeline.time)-1
	dstart = dblarr(n)
	dstop = dblarr(n)
	ipos = 0
	state = 0
	for i=0L,n do begin
	    if mask(i) ne state then begin
		if state eq 0 then begin
			dstart(ipos) = (timeline.time(i)-0.5)>0
			state = 1
		    end else begin
		    	dstop(ipos) = timeline.time(i-1)+0.5
			ipos = ipos + 1
			state = 0
		end
	    end
	    if (state eq 1) and (i lt n) then begin
	    	if (timeline.time(i+1)-timeline.time(i)) gt 2.0 then begin
			dstop(ipos) = timeline.time(i)+0.5
			ipos = ipos + 1
			state = 0
		end
	    end
	end
	if state eq 1 then begin
		dstop(ipos) = timeline.time(n-1)+0.5
		ipos = ipos + 1
	end
	if ipos gt 0 then gti_tab = {start:dstart(0:ipos-1),$
				     stop:dstop(0:ipos-1)} $
		     else gti_tab = {start:0.0d0,stop:0.0d0}

        
        FOR channel=0, 7 DO BEGIN
           IF ((channel MOD 4) NE 0) THEN BEGIN
              IF (info.ycents_qual(channel) GT 0) THEN BEGIN
                 sxaddpar,  header,'YCENT'+string(channel,'(I1.1)'), info.ycents(channel)
                 sxaddpar,  header,'YQUAL'+string(channel,'(I1.1)'), (['', 'LOW', 'MEDIUM', 'HIGH'])[(info.ycents_qual)[channel]]
              ENDIF 
           ENDIF

        ENDFOR

;
; write file
;
	mwrfits,0,file,header,/create
	mwrfits,events,file,event_header,/no_types,/no_comment
	mwrfits,gti_tab,file,(*input.gti_header(0)),/no_types,/no_comment
	mwrfits,timeline,file,timeline_header,/no_types,/no_comment	
end
;=========================================================== CF_EDIT_REGISTER
;
; Routine to extract a spectrum
;
pro cf_edit_register,info,menu_number,input,selection
;
	channel = ([0,1,2,3,5,6,7])[menu_number]
	widget_control,/hourglass
	title = (['','LiF HIRS','LiF MDRS','LiF LWRS','LiF PINH', $
		     'SiC HIRS','SiC MDRS','SiC LWRS','SiC PINH'])[channel]
;
; determine wavelengths
;

;	w = (*input.event[0]).lambda
;	good = where(w gt 0)
;	w = w(good)
;	wmin = min(w,max=wmax)
;	wmin = fix(wmin)
;	wmax = ceil(wmax)


        
        detector = strtrim(sxpar((*input.header(0)),'detector'),2)
        
        case detector of
           '1A': begin
              wmin = [985,997]
              wmax = [1085,1097]
           end
           '1B': begin
              wmin = [1090,900]
              wmax = [1190,1000]
           end
           '2A': begin
              wmin = [1085,910]
              wmax = [1185,1010]
           end
           '2B': begin
              wmin = [977,1010]
              wmax = [1077,1110]
           end
        endcase
        
        IF (channel LE 4) THEN BEGIN
           wmin = wmin[0]
           wmax = wmax[0]
        ENDIF ELSE BEGIN
           wmin = wmin[1]
           wmax = wmax[1]
        ENDELSE
 

	dw = info.deltaw > 0.005
	n = round((wmax-wmin)/dw)
	nfiles = input.nfiles
	wave = dindgen(n)*dw+wmin
	wave_all = rebin(wave,n,nfiles,/sample)
	flux_all = fltarr(n,nfiles)
	woffset = dblarr(n)
	exptimes = fltarr(n)
;
; loop on files and extract each individual spectra
;	
	for i=0,input.nfiles-1 do begin

;
; select events using time flags and GTI mask
;
	    cf_edit_select,input,i,selection,mask,channel=channel, $
	    		exptime=exptime
	    exptimes(i) = exptime
	    good = where(mask,ngood)
	    if ngood eq 0 then goto,nexti
	    w = (*input.event[i]).lambda[good]
	    bin = round((w-wmin)/dw)
	    if info.flux eq 0 then begin
	    	flux_all(0,i) = histogram(bin,min=0,max=n-1,input=flux)
	      end else begin
	        flux = fltarr(n)
		cf_edit_drizzle,flux,bin,(*input.event[i]).ergcm2[good]
		flux_all(0,i) = flux/(exptime>1)
	    end
nexti:
	end
		
;
; get list of file names
;
	files = strarr(nfiles)
	for i=0,nfiles-1 do begin
		fdecomp,input.files(i),disk,dir,name
		files(i) = name
	end
;
; determine which spectra to register
;
next_group:
	if nfiles gt 10 then begin
		if channel lt 4 then woffsets = input.woffset_lif $
				else woffsets = input.woffset_sic
		names = files
		for i=0,n_elements(names)-1 do $
			if woffsets(i) ne 0 then names(i) = names(i) + $
						string(woffsets(i),'(F8.4)')
		
		cf_edit_selector,names,'Select up to 10 files',iselect, $
			group=info.base
		if iselect(0) eq -1 then return
	end else iselect = indgen(nfiles)
;
; compute wavelength offsets
;
	ones = replicate(1,n)
	for i=0,n_elements(iselect)-1 do begin
	    k = iselect(i)

	    xregister_1d,wave_all(*,k),flux_all(*,k),ones,ones, $
		    	title=files(k),xtitle='Wavelength',ytitle='Flux', $
			group = info.base, init = i eq 0, weight = exptimes(k)
	end
	xregister_1d,/process,woffset=woffset
;
; save and apply wavelength offsets
;
	widget_control,/hourglass

	for i=0,n_elements(iselect)-1 do begin
	    woff = woffset(i)
	    k = iselect(i)
	    if woff ne 0.0 then begin
		wave_all(0,k) = wave_all(*,k) + woff
	        w = (*input.event[k]).lambda
		g = where((*input.event[k]).channel eq channel)
		w(g) = w(g) + woff
		(*input.event[k]).lambda = w
		if channel le 4 then  $
			input.woffset_lif(k) = input.woffset_lif(k)+woff $
		else input.woffset_sic(k) = input.woffset_sic(k)+woff
	    end
	end
	widget_control,/hourglass
	
	if nfiles gt 10 then goto,next_group
end
;============================================================ CF_EDIT_SELECTOR
;
; Routine to select file(s) to process
;
pro cf_edit_selector,list,command,iselect,group=group

	if n_elements(only_one) eq 0 then only_one=0
	tlb = widget_base(/col,group=group,title='File Selection Tool')
	base = widget_button(tlb,uvalue='DONE',value='DONE')
	base = widget_label(tlb,value=command)
	bgroup = cw_bgroup(tlb,list,/column,/scroll,/nonexclusive, $
		x_scroll_size=250,y_scroll_size=400,UVALUE='SELECT')
;	
	pselect = ptr_new(replicate(0,n_elements(list)))
	info = {bgroup:bgroup,pselect:pselect}
;
; create widget and execute
;
	widget_control,tlb,/realize
	widget_control,tlb,set_uvalue=info,/no_copy
	xmanager,'cf_edit_selector',tlb
;
; extract results and clean up
;
	iselect = where(*pselect eq 1)
	ptr_free,pselect
	return
end
pro cf_edit_selector_event,event

	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'DONE' then begin
		widget_control,event.top,get_uvalue=info
		widget_control,info.bgroup,get_value=values
		*info.pselect = values
		widget_control,event.top,set_uvalue=info
		widget_control,event.top,/destroy
		return
	end		
end
;========================================================== CF_EDIT_TTAG_SELECT
;
; Routine to make list of good time tag events
;
;
pro cf_edit_ttag_select,info,input,selection,x,y,t
;
; determine type of image
;	0 = raw
;	1 = FARF
;	2 = Final
;	3 = wave/y
;
	widget_control,/hourglass
	image_type = widget_info(info.display_base,/droplist_select)
;
; count total number of events
;
	nevents = 0L
	for i=0,input.nfiles-1 do nevents = nevents + $
			n_elements((*input.event[i]).time)
	x = intarr(nevents)
	y = intarr(nevents)
	t = fltarr(nevents)
	n = 0
;
; loop on images
;
	for i=0,input.nfiles-1 do begin
;
; select events using time flags and GTI mask
;
		cf_edit_select,input,i,selection,mask,exptime=exptime
		
		good = where(mask,ngood)
		if ngood eq 0 then goto,nexti

		case image_type of
		    0: begin
		    	x(n) = round((*input.event[i]).xraw[good])
			y(n) = round((*input.event[i]).yraw[good])
		       end
		    1: begin
		    	x(n) = round((*input.event[i]).xfarf[good])
			y(n) = round((*input.event[i]).yfarf[good])
		       end
		    2: begin
		    	x(n) = round((*input.event[i]).x[good])
			y(n) = round((*input.event[i]).y[good])
		       end
		    3: begin
		        if i eq 0 then coef=[[0d0,0d0],[0d0,0d0]]
			cf_edit_wtox,detector,input,i,good,coef,xx
			x(n) = round(x)
			y(n) = round((*input.event[i]).y[good])
		       end
		endcase
		
		t(n) = (*input.event[i]).time[good] + input.timestart[i]
		n = n+ngood
nexti:
	end
	if n gt 0 then begin
		x = x(0:n-1)
		y = y(0:n-1)
		t = t(0:n-1)
	    end else begin
	    	x = 0
		y = 0
		t = 0
	end
	widget_control,/hourglass
end

;================================================================= CF_EDIT_TTAG
;
; Routine to analyze ttag events
;
pro cf_edit_ttag,filename,x_in,y_in,time_in,xbin_in,ybin_in, $
	xrange,yrange,group=group
;
common cf_edit_ttag_common,x,y,t,xbin,ybin,mbase, $
	base,file,h,time,xcent,ycent,counts,tcounts,title
;
; initialize common
;
	if xregistered('xttag_fuse') then widget_control,base.main,/destroy
	time = [time_in(0),time_in(n_elements(time_in)-1)]
	file = filename
	xbin = xbin_in
	ybin = ybin_in
	x = temporary(x_in)
	y = temporary(y_in)
	t = temporary(time_in)
	
;
; create widget layout
;
	main = widget_base(/col,group=group,title=file)
	menu = widget_base(main,/row)
	exit = widget_button(menu,uvalue='EXIT',value='EXIT')
	movie = widget_button(menu,uvalue='MOVIE',value='Movie')
	main1 = widget_base(main,/col,/frame)
	b = widget_base(main1,/row)
	x1 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(0), $
			/integer,/return_events,title='X Range: ')
	x2 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(1), $
			/integer,/return_events,title=' ')
	b = widget_base(main1,/row)
	y1 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(0), $
			/integer,/return_events,title='Y Range: ')
	y2 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(1), $
			/integer,/return_events,title=' ')
	deltat = cw_field(main1,uvalue='PARAM',xsize=6,value=120,/integer, $
			/return_events,title='Time Intervals (Seconds):')
	minc = cw_field(main1,uvalue='PARAM',xsize=6,value=20,/integer, $
			/return_events,title='Min counts needed to centroid:') 
	process = widget_button(main,uvalue='PROCESS',value='PROCESS')
	main2 = widget_base(main,/col,frame=1)
	plotx = widget_button(main2,value='  PLOT X Centroids  ',uvalue='PLOTX')
	ploty = widget_button(main2,value='  PLOT Y Centroids  ',uvalue='PLOTY')
	plotc = widget_button(main2,value='  PLOT Counts    ',uvalue='PLOTC')
	plottc = widget_button(main2,value='      PLOT Total Counts      ', $
				uvalue='PLOTTC')
	window = widget_draw(main,xsize=300,ysize=300,uvalue='WINDOW',/motion)
;
; start widget
;	
	widget_control,main,/realize
	widget_control,main2,sensitive=0
	widget_control,window,get_v = id
	base = {main:main,exit:exit,main1:main1,x1:x1,x2:x2,y1:y1,y2:y2, $
		deltat:deltat,minc:minc,main2:main2,id:id}
	xmanager,'cf_edit_ttag',main,/no_block
	return
	end
;
;
;============================================================ CF_EDIT_TTAG_EVENT
;
; Subroutine of cf_edit for processing time tag data
;
;
; event driver 
;
pro cf_edit_ttag_event,event

common cf_edit_ttag_common,x,y,t,xbin,ybin,mbase, $
	base,file,h,time,xcent,ycent,counts,tcounts,title

	widget_control,event.id,get_uvalue=v
	uvalue = gettok(v,'_')
	utype = v
	widget_control,base.x1,get_v=x1
	widget_control,base.x2,get_v=x2
	widget_control,base.y1,get_v=y1
	widget_control,base.y2,get_v=y2
	if x2 lt x1 then begin
		temp = x1 & x1=x2 & x2 = temp
	end
	if y2 lt y1 then begin
		temp = y1 & y1=y2 & y2 = temp
	end
	widget_control,base.deltat,get_v=delt
	widget_control,base.minc,get_v=minc
	loadct,5,/silent
	case uvalue of
	'EXIT': begin
		time = 0 & xcent = 0 & ycent = 0
		counts = 0 & tcounts = 0 & base = 0
		widget_control,event.top,/destroy
		end
	'MOVIE': begin
		widget_control,base.x1,get_v=x1
		widget_control,base.x2,get_v=x2
		widget_control,base.y1,get_v=y1
		widget_control,base.y2,get_v=y2
		cf_edit_movie,[x1,x2],[y1,y2],group=event.top
		return
		end
	'PARAM': widget_control,base.main2,sensitive=0
	'PROCESS': begin
		if delt lt 1 then delt = 1
		if minc lt 1 then minc = 1
		widget_control,base.x1,set_v=x1
		widget_control,base.x2,set_v=x2
		widget_control,base.y1,set_v=y1
		widget_control,base.y2,set_v=y2
		widget_control,base.deltat,set_v=delt
		widget_control,base.minc,set_v=minc
		widget_control,/hourglass
		cf_edit_ttag_centroid,t,x,y,time,xcent,ycent,counts,tcounts, $
			image,interval = float(delt), mincounts = minc, $
			xrange = [x1,x2], yrange = [y1,y2]
		widget_control,base.main2,sensitive=1
		fdecomp,file,disk,dir,name
		title = name + ' ('+ strtrim(x1,2)+':'+strtrim(x2,2)+','+ $
				     strtrim(y1,2)+':'+strtrim(y2,2)+')'
		wset,base.id
		tvscl,frebin(alog10(image>0.1),300,300)
		widget_control,/hourglass
		end
	'PLOTX': begin
		widget_control,base.minc,get_v=minc
		good = where(counts gt minc,ngood)
		if ngood lt 1 then begin
			istat = dialog_message('No Valid Centroids Computed', $
					/error,dialog_parent=event.top)
			return
		end
		yrange = [min(xcent(good))-20,max(xcent(good))+20]
		lineplot,time,xcent,title=title+' X Centroid', $
			yrange=yrange,xtitle='time (seconds)',min_val=0.1
		end
	'PLOTY': begin
		widget_control,base.minc,get_v=minc
		good = where(counts gt minc,ngood)
		if ngood lt 1 then begin
			istat = dialog_message('No Valid Centroids Computed', $
					/error,dialog_parent=event.top)
			return
		end
		yrange = [min(ycent(good))-20,max(ycent(good))+20]
		lineplot,time,ycent,title=title+' Y Centroid', $
			  yrange=yrange,xtitle='time (seconds)',min_val=0.1
		end
	'PLOTC': begin
		lineplot,time,counts,title=title+' Counts', $
				xtitle='time (seconds)'	
		end
	'PLOTTC': begin
		lineplot,time,tcounts,title=title+' Total Counts', $
				xtitle='time (seconds)'	
		end
	'WINDOW':
	endcase
	return
end
;======================================================== CF_EDIT_TTAG_CENTROID
;
; Routine to compute centroids
;
pro cf_edit_ttag_centroid,time,x,y,tout,xcent,ycent,counts,tcounts,image, $
	interval=interval, $
	xrange=xrange,yrange=yrange,tmin=tmin,tmax=tmax,mincounts=mincounts

	if n_elements(xrange) eq 0 then xrange = [0,16383]
	if n_elements(yrange) eq 0 then yrange = [0,1023]
	if n_elements(interval) eq 0 then interval = 60.0d0
	if n_elements(tmin) eq 0 then tmin = 0.0
	if n_elements(tmax) eq 0 then tmax = max(time)
	if n_elements(mincounts) eq 0 then mincounts = 20
;
; select proper x and y values by range
;
	keep = where((x ge xrange(0)) and (x le xrange(1)) and $
		     (y ge yrange(0)) and (y le yrange(1)) and $
		     (time ge tmin) and (time le tmax),nkeep)
	if nkeep le 1 then begin
		istat = dialog_message('TTAG_CENTROID: ERROR - no events' + $
			' in specified position/time range',/error)
		retall
	endif
	tt = time(keep) - tmin
	xx = x(keep)
	yy = y(keep)
;
; bin events into bins specified by interval.
;
	nbins = long((tmax-tmin)/interval)>1
	tout = findgen(nbins)*interval + (interval/2.)
	tabinv,tt,tout-interval/2.0,index1
	tabinv,tt,tout+interval/2.0,index2
	index1 = long(index1)+1
	index2 = long(index2)
	xcent = fltarr(nbins)
	ycent = fltarr(nbins)
	counts = (index2-index1+1)>0
;
; compute centroid for each interval with more than mincounts
;
	for i=0L,nbins-1 do begin
		if counts(i) gt mincounts then begin	
			xcent(i) = total(xx(index1(i):index2(i)))/counts(i)			
			ycent(i) = total(yy(index1(i):index2(i)))/counts(i)			
		end
	end
	tout = tout + tmin
;
; compute counts in the time bins for the whole image
;
	tabinv,time,tout-interval/2.0,index1
	tabinv,time,tout+interval/2.0,index2
	index1 = long(index1)+1
	index2 = long(index2)
	tcounts = (index2-index1+1)
	image = hist_2d(xx,yy,min1=xrange(0),max1=xrange(1), $
			      min2=yrange(0),max2=yrange(1))
return
end
;=========================================================== CF_EDIT_MOVIE
;
; Movie creation GUI
;
pro cf_edit_movie,xrange,yrange,group=group

common cf_edit_ttag_common,x,y,t,xbin,ybin,mbase, $
	base,file,h,time,xcent,ycent,counts,tcounts,title
;
; create widget layout
;

	main = widget_base(/col,group=group,title='FUSE MOVIE CREATOR')

	exit = widget_button(main,uvalue='EXIT',value='EXIT')
;
; region selection
;
	main1 = widget_base(main,/col,/frame,group=group)
	label = widget_label(main1,value='RANGE SELECTION')
	b = widget_base(main1,/row)
	x1 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(0), $
			/integer,/return_events,title='X Range: ')
	x2 = cw_field(b,uvalue='PARAM',xsize=8,value=xrange(1), $
			/integer,/return_events,title=' to ')
	button = widget_button(b,uvalue='FULLX',value='Full')
	b = widget_base(main1,/row)
	y1 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(0), $
			/integer,/return_events,title='Y Range: ')
	y2 = cw_field(b,uvalue='PARAM',xsize=8,value=yrange(1), $
			/integer,/return_events,title=' to ')
	button = widget_button(b,uvalue='FULLY',value='Full')
	
	label = widget_label(main1,value='Time Period to display in seconds')
	label = widget_label(main1,value='from start of observation')
	tmin = min(t,max=tmax)
	tmin = long(tmin)
	tmax = long(tmax+1)
	b = widget_base(main1,/row)
	t1 = cw_field(b,uvalue='PARAM',xsize=8,value=tmin, $
			/long,/return_events,title='T Range: ')
	t2 = cw_field(b,uvalue='PARAM',xsize=8,value=tmax, $
			/long,/return_events,title=' to ')
	button = widget_button(b,uvalue='FULLT',value='Full')
;
; binning
;
	main2 = widget_base(main,/col,/frame)
	label = widget_label(main2,value='BINNING SELECTION')
	b = widget_base(main2,/row)
	binx = cw_field(b,uvalue='PARAM',xsize=8,value=1, $
			/integer,/return_events,title='Bin X: ')
	biny = cw_field(b,uvalue='PARAM',xsize=8,value=1, $
			/integer,/return_events,title='Bin Y: ')
	magnify = cw_field(main2,uvalue='PARAM',xsize=4,title='Zoom Factor:', $
			/integer,/return_events,value=1)
	b = widget_base(main2,/row)
	v = float(round((tmax-tmin)/100>1))
	tint = cw_field(b,uvalue='PARAM',xsize=8,value=v,/float, $
			/return_events,title='Time Interval: ')
	label = widget_label(b,value=' Seconds')
;
; Estimated Movie Size
;
	main3 = widget_base(main,/col,/frame)
	label = widget_label(main3,value='The estimated movie size in '+ $
						'Megabytes will be:')
	mb = (long(xrange(1)-xrange(0)+1) * $
	      long(yrange(1)-yrange(0)+1) * 100.0)/1000000.
	mvsize = widget_label(main3,/frame,value=string(mb,'(F20.1)'))
	label = widget_label(main3,value='The value must be less than 128.')
	label = widget_label(main3,value='To decrease: increase bin sizes or')
	label = widget_label(main3,value='or decrease range sizes')
	fsize = widget_label(main3,value='Frame Size:              '+ $
		strtrim(xrange(1)-xrange(0)+1,2) + ' x ' + $
		strtrim(yrange(1)-yrange(0)+1,2))
	nframes = widget_label(main3,value='Number of Frames:         100') 
;
; Movie display range
;
;
	main4 = widget_base(main,/col,/frame)
	label = widget_label(main4,value='MOVIE CONTRAST')
	b = widget_base(main4,/row)
	minval = cw_field(b,uvalue='PARAM',xsize=5,value=0,/integer, $
			/return_events,title='Min counts: ')
	maxval = cw_field(b,uvalue='PARAM',xsize=5,value=10,/integer, $
			/return_events,title='Max counts: ')
	create = widget_button(main,uvalue='CREATE',value='LIGHTS,   ' + $
			'CAMERA,   ACTION')
;
; start widget
;
	widget_control,main,/realize
	mbase = {main:main, x1:x1, x2:x2, y1:y1, y2:y2, t1:t1, t2:t2,  $
		binx:binx, biny:biny, magnify:magnify, tint:tint,  $
		mvsize:mvsize, fsize:fsize, nframes:nframes, minval:minval, $
		maxval:maxval, tmin:tmin, tmax:tmax}
	widget_control,main,set_uvalue=base	
	xmanager,'xttag_fuse_movie',main,/no_block
	return
	end

;====================================================== CF_EDIT_MOVIE_EVENT
;
;
; Event driver for movie creation routine
;
pro xttag_fuse_movie_event,event
	COMMON XInterAnimate_com, topbase, animatebase, pwin, mpegID

common cf_edit_ttag_common,x,y,t,xbin,ybin,mbase, $
	base,file,h,time,xcent,ycent,counts,tcounts,title

	widget_control,event.top,get_uvalue=base
	widget_control,event.id,get_uvalue=uvalue
	if uvalue eq 'EXIT' then begin
		widget_control,event.top,/destroy
		return
	end
;
; process Full Buttons
;
	if uvalue eq 'FULLX' then begin
		widget_control,mbase.x1,set_v=0
		widget_control,mbase.x2,set_v=16383
		widget_control,mbase.magnify,set_v=1
		widget_control,mbase.binx,set_v=64
	endif
	
	if uvalue eq 'FULLY' then begin
		widget_control,mbase.y1,set_v=0
		widget_control,mbase.y2,set_v=1023
		widget_control,mbase.magnify,set_v=1
		widget_control,mbase.biny,set_v=16
	endif
	
	if uvalue eq 'FULLT' then begin
		widget_control,mbase.t1,set_v=base.tmin
		widget_control,mbase.t2,set_v=base.tmax
	endif

;
; extract parameters
;
	widget_control,mbase.x1,get_v=x1
	widget_control,mbase.x2,get_v=x2
	widget_control,mbase.y1,get_v=y1
	widget_control,mbase.y2,get_v=y2
	widget_control,mbase.t1,get_v=t1
	widget_control,mbase.t2,get_v=t2
	widget_control,mbase.binx,get_v=binx
	widget_control,mbase.biny,get_v=biny
	widget_control,mbase.magnify,get_v=magnify
	widget_control,mbase.tint,get_v=tint
	widget_control,mbase.minval,get_v=minval
	widget_control,mbase.maxval,get_v=maxval
;
; idiot testing
;
	if x1 lt 0 then begin
		x1 = 0
		widget_control,mbase.x1,set_v=x1
	end
	if x2 gt 16383 then begin
		x2 = 16383
		widget_control,mbase.x2,set_v=x2
	end
	if y1 lt 0 then begin
		y1 = 0
		widget_control,mbase.y1,set_v=y1
	end
	if y2 gt 1023 then begin
		y2 = 1023
		widget_control,mbase.y2,set_v=y2
	end
	if t1 lt 0 then begin
		t1 = 0
		widget_control,mbase.t1,set_v=t1
	end
	if t2 gt mbase.tmax then begin
		t2 = mbase.tmax
		widget_control,mbase.t2,set_v=t2
	end
	if magnify lt 1 then begin
		magnify = 1
		widget_control,mbase.magnify,set_v=magnify
	end
	if tint le 0 then begin
		tint = 1.0
		widget_control,mbase.tint,set_v=tint
	end
	if binx le 0 then begin
		binx = 1
		widget_control,mbase.binx,set_v=binx
	end
	if biny le 0 then begin
		biny = 1
		widget_control,mbase.biny,set_v=biny
	end
;
; determine movie size
;
	nframes = long((t2-t1)/tint)>1
	xfsize = (x2-x1+1)/binx*magnify
	yfsize = (y2-y1+1)/biny*magnify
	mvsize = float(nframes)*xfsize*yfsize/1000000.0
	widget_control,mbase.mvsize,set_v=string(mvsize,'(F8.1)')
	widget_control,mbase.fsize,set_v = 'Frame Size:     '+ $
		strtrim(xfsize,2)+ ' x '+ strtrim(yfsize,2)
	widget_control,mbase.nframes,set_v = 'Number of Frames:    '+ $
		strtrim(nframes,2)
;
; More idiot testing
;
	if uvalue ne 'CREATE' then return
	if n_elements(topbase) gt 0 then xinteranimate,/close
	if mvsize gt 128 then begin
		istat = dialog_message('Movie size is too big',/error, $
				dialog_parent=event.top)
		return
	end
	if (xfsize gt 1024) then begin
		istat = dialog_message('Movie X frame size is too big', $
			dialog_parent = event.top,/error)
		return
	endif
	if (yfsize gt 900) then begin
		istat = dialog_message('Movie Y frame size is too big', $
			dialog_parent = event.top,/error)
		return
	endif
	if xfsize lt 1 then begin
		istat = dialog_message('Movie X frame size is too small', $
			dialog_parent = event.top,/error)
	end
	if yfsize lt 1 then begin
		istat = dialog_message('Movie Y frame size is too small', $
			dialog_parent = event.top,/error)
	end
;
; set up animation widget
;
	xinteranimate,group=event.top,set=[xfsize,yfsize,nframes], $
		/showload,/track
	cf_edit_make_movie,t,x,y,range = [x1,x2,y1,y2], tmin=t1,tmax=t2, $
		interval=tint,xbinsize=binx,ybinsize=biny,magnify=magnify, $
		minval=minval,maxval=maxval
	xinteranimate,group=event.top
		
	return
	end
;========================================================== CF_MAKE_EDIT_MOVIE
;
; routine to create movie
;
pro cf_edit_make_movie,time,x,y,range=range,tmin=tmin,tmax=tmax, $
	interval=interval,xbinsize=xbinsize,ybinsize=ybinsize, $
	magnify=magnify,minval=minval,maxval=maxval
;
; INPUTS:
;	time - vector of time values 
;	x - vector of x positions 
;	y - vector of y positions 
;
; OPTIONAL KEYWORD INPUTS:
;
;	range = range of pixel positions in unbinnned pixel
;		locations to use: [xmin,xmax,ymin,ymax]
;		(default is the entire image)
;	tmin = minimum time to consider (default = min(time))
;	tmax = maximum time to consider (default = max(time))
;	interval = time interval between movie frames - default is
;		(tmax - tmin)/20
;	xbinsize = binning factor in x (default = 1)
;	ybinsize = binning factor in y (default = 1)
;	magnify = magnfication factor (default = 1)
;	minval = minimum display value for contrast control
;	maxval = maximum display value
;
;
; set defaults
;
	if n_elements(range) eq 0 then range = [0,16383,0,1023]
	if n_elements(tmin) eq 0 then tmin = min(time)
	if n_elements(tmax) eq 0 then tmax = max(time)
	if n_elements(interval) eq 0 then interval = (tmax-tmin)/40
	if n_elements(xbinsize) eq 0 then xbinsize = 2
	if n_elements(ybinsize) eq 0 then ybinsize = 2
	if n_elements(magnify) eq 0 then magnify = 1
	if n_elements(minval) eq 0 then minval = 0
	if n_elements(maxval) eq 0 then maxval = !d.n_colors-1
;
; determine size of output data cube
;
	xmin = range(0)
	xmax = range(1)
	ymin = range(2)
	ymax = range(3)
	ns = long((xmax-xmin+1)/xbinsize)
	nl = long((ymax-ymin+1)/ybinsize)
	nt = long((tmax-tmin)/interval)
	n= ns*nl*nt				;number of elements in cube
;
; adjust maximums to account for partial bins
;
	ymax = ymin + nl*ybinsize - 1
	xmax = xmin + ns*xbinsize - 1
	timemax = tmin + interval*nt
;
; create vector of frame times
;
	framet = findgen(nt)*interval + (tmin + interval/2.0)
;
; find first and last index for each time bin
;
;
	tabinv,time,framet-interval/2.0,index1
	tabinv,time,framet+interval/2.0,index2
	index1 = long(index1)+1
	index2 = long(index2)
;
; loop on frames
;
	for i = 0,nt-1 do begin
	    i1 = index1(i)
	    i2 = index2(i)
	    if i2 ge i1 then begin
	    	xx = x(i1:i2)
		yy = y(i1:i2)
		good = where((xx ge xmin) and (xx le xmax) and $
			     (yy ge ymin) and (yy le ymax),ngood)
	    end else ngood = 0
	    if ngood gt 0 then begin
	    	xx = xx(good)-xmin
		yy = yy(good)-ymin
		image = hist_2d(xx/xbinsize,yy/ybinsize,min1=0,max1=ns-1, $
			min2=0,max2=nl-1)
		image = bytscl(image,top=!d.n_colors-1,min=minval,max=maxval)
	    end else image = bytarr(ns,nl)
	    if magnify gt 1 then $
	    		image = rebin(image,ns*magnify,nl*magnify,/sample)
	    xinteranimate,frame=i,image=image
	 end
	return
end

;====================================================================== CF_EDIT
;
; Main Routine
;
pro cf_edit
	common cf_edit_common,info,image,little_image,input,selection

	
	if xregistered('cf_edit') then return		

;	Switch from TrueColor to PseudoColor if running on a Mac
	if !version.arch eq 'ppc' then begin
		device, true_color=24
		window, /free, /pixmap, colors=-10
		wdelete, !d.window
		device, decomposed=0, retain=2, set_character_size=[8,10]
		device, get_visual_depth=depth
	endif

; create widget layout
;
	base = widget_base(/col,group=0,uvalue='MAIN',/tracking_events, $
		title='CF_EDIT Main Menu')
	widget_control,base
;
; Button Bar
;
	base1 = widget_base(base,/row)
	desc = ['1\FILE','0\Read IDF File(s)','0\Write IDF file', $
		'0\Default Parameters','1\PS output','0\B/W', $
		'0\B/W Reversed','2\Color','2\EXIT']
	button = cw_pdmenu(base1,desc,uvalue='FILE')
	basebar = widget_base(base1,/row)
	button = widget_button(basebar,uvalue='COLORS',value='Colors')
	scale_base = widget_droplist(basebar,uvalue='SCALE',title='Scale:', $
			value=['Linear','Sqrt','Log','Hist. Eg.'])
	display_base = widget_droplist(basebar,uvalue='DISPLAY', $
			title='Display:', $
			value=['Raw X/Y','XFARF/YFARF','Final X/Y', $
				'Wavelength/Y'])
	widget_control,display_base,set_droplist_select=2

	desc = ['1\Plot','0\Row','0\Column','0\Row Sum','0\Column Sum', $
		'2\PHA']
	button = cw_pdmenu(basebar,desc,uvalue='PLOT')
	
	button = widget_button(basebar,uvalue='OVERLAY',value='Overlay')

	button = widget_button(basebar,value='Event Selection', $
			uvalue='EVENT_SELECTION')
	
	desc = ['1\GTI Selection', $
		'0\Time since sunrise', $
		'0\Time since sunset', $
		'0\Limb Angle ', $
		'0\Spacecraft Longitude ', $
		'0\Spacecraft Latitute ', $
		'0\Orbital Velocity ', $
		'0\High Voltage ', $
		'0\LiF Count rate ', $
		'0\SiC Count rate ', $
		'0\FEC count rate ', $
		'0\AIC count rate ', $
		'0\Background count rate ', $
		'0\Y centroid of LiF target Spectrum', $
		'2\Y centroid of SiC target Spectrum']
	button = cw_pdmenu(basebar,desc,uvalue='GTI SELECTION')

	desc = ['1\Extract Spectrum', $
		'0\LiF HIRS','0\LiF MDRS','0\LiF LWRS', $
		'0\SiC HIRS','0\SiC MDRS','2\SiC LWRS']
	button = cw_pdmenu(basebar,desc,uvalue='EXTRACT SPECTRUM')
	
	basebar2 = widget_base(base,/row)
        
	desc = ['1\Background','0\Adjust Region','0\Create New Region', $
		'2\Remove Region']
	button = cw_pdmenu(basebar2,desc,uvalue='BACKGROUND')
	desc = ['1\Header','0\Display','2\Add Comments']
	button = cw_pdmenu(basebar2,desc,uvalue='HEADER')
	desc = ['1\Register', $
		'0\LiF HIRS','0\LiF MDRS','0\LiF LWRS', $
		'0\SiC HIRS','0\SiC MDRS','2\SiC LWRS']
	button = cw_pdmenu(basebar2,desc,uvalue='REGISTER')
	
	
	desc = ['1\Y CENTROID', $
		'1\LiF HIRS','0\Target events only','0\Airglow events only','0\Default','2\User select', $
		'1\LiF MDRS','0\Target events only','0\Airglow events only','0\Default','2\User select', $
		'1\LiF LWRS','0\Target events only','0\Airglow events only','0\Default','2\User select', $ 
		'1\SiC HIRS','0\Target events only','0\Airglow events only','0\Default','2\User select', $
		'1\SiC MDRS','0\Target events only','0\Airglow events only','0\Default','2\User select', $
		'1\SiC LWRS','0\Target events only','0\Airglow events only','0\Default','2\User select', $
		'2\']
	button = cw_pdmenu(basebar2,desc,uvalue='CALCULATE Y_CENTROID')

	ttag_button = widget_button(basebar2,value='TTAG Analysis', $
				uvalue='TTAG')

	exptime_field = cw_field(basebar2,uvalue='EXPTIME',value=exptime, $
                title='Exptime:   ',xsize=13,/return_events,/long)
	basewindows = widget_base(base,/row)
	basebig = widget_base(basewindows,/col)
	basex = widget_base(basebig,/row)
	big_position = widget_label(basex,value='XXXXX:XXXX XXXXX:XXXX')
	
	message = widget_label(basex,xsize=500,value = '    ')
	big_window = widget_draw(basebig,uvalue='BIG_WINDOW', $
			xsize=750,ysize=512,x_scroll_size=700, $
			y_scroll_size=542,/button_events,/motion, $
			/viewport_events)
	base2 = widget_base(basewindows,/col)
	desc = ['1\Zoom Factor','0\ 1','0\ 2','0\ 3','0\ 4','0\ 5','0\ 6', $
		'0\ 7','0\ 8','0\ 9','0\10','0\16','2\32']
	button = cw_pdmenu(base2,desc,uvalue='ZOOM',/return_full_name)
	zoom_window = widget_draw(base2,uvalue='ZOOM_WINDOW',retain=2, $
			xsize=256,ysize=256,/motion,/button_events)
	zoom_position = widget_label(base2,value='XXXXX:XXXX XXXXX:XXXX')

	base2a = widget_base(base2,/col,/frame)

	freeze = widget_button(base2a,uvalue='FREEZE',value='Freeze Min/Max')
        min_field = cw_field(base2a,/row,uvalue='MIN_FIELD',value=omin, $
                title='Min: ',xsize=13,/return_events,/float)
        max_field = cw_field(base2a,/row,uvalue='MAX_FIELD',value=omax, $
                title='Max: ',xsize=13,/return_events,/float)
	button = widget_button(base2a,uvalue='RESET',value='Reset Min/Max')

	base2b = widget_base(base2,/col,/frame)
	x_field = cw_field(base2b,/row,uvalue='X_FIELD',value=-1, $
                title='X:   ',xsize=13,/return_events,/long)
	y_field = cw_field(base2b,/row,uvalue='Y_FIELD',value=-1, $
                title='Y:   ',xsize=13,/return_events,/long)
	val_field = cw_field(base2b,/row,uvalue='VAL_FIELD',value=-1, $
                title='Val: ',xsize=13,/return_events,/float)
	
	
	
	little_window = widget_draw(base,uvalue='LITTLE_WINDOW',retain=2, $
			xsize=1024,ysize=128,/button_events,/motion)


	

;
; create widget
;
	widget_control,base,/realize
        widget_control,big_window,get_value=big_id
        widget_control,little_window,get_value=little_id
        widget_control,zoom_window,get_value=zoom_id
	tvlct,rsave,gsave,bsave,/get
;
; create info structure
;
	info = {base:base, basebar:basebar, basebar2:basebar2, $
		ttag_button:ttag_button, $
		big_window:big_window, big_id:big_id, $
		little_window:little_window, little_id:little_id, $
		zoom_window:zoom_window, zoom_id:zoom_id, $
		zoom_factor:5, $
		cursor_state:'NONE', cursor_substate:'', down:0, $
		xsave:-1, ysave:-1, x1:-1, y1:-1, $
		xoffzoom:0, yoffzoom:0, $
		rsave:rsave, gsave:gsave, bsave:bsave, $
		message:message, $
		x_field:x_field, y_field:y_field, $
		val_field:val_field, $
		exptime_field:exptime_field, $
		min_field:min_field, max_field:max_field, $
		big_position:big_position, zoom_position:zoom_position, $
		freeze:freeze, $
		scale_base:scale_base, $
		display_base:display_base, $
		xbin:2, ybin:2, ns:0, nl:0, $
		deltaw:0.013, flux:1, $
		omin:0.0, omax:0.0, $
		caldir:getenv('CF_CALDIR'), $
		overlay:intarr(5), $
                ycents:dblarr(8), ycents_qual:intarr(8), ycents_default:dblarr(8), $
		bpos:intarr(20),nback:0,back_edit:-1}
		
	input = {nfiles:0,directory:''}

	widget_control,basebar,sensitive = 0
	widget_control,basebar2,sensitive = 0

	device	;, decomp=0
 	image = dist(1024)
 	cf_edit_display,info,input,image,little_image,/new
	
	xmanager,'cf_edit',base,/no_block
	return
end
