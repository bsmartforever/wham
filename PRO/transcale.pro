pro transcale, infile, date, calib,scale, var, sum_file=sum_file, mkplt=mkplt


	npar = N_params()
	
	if ( npar LT 3 ) then begin
	
	  print,'Syntax ---- plt_calibrators, infile, date, calib [, /sum_file, /mkplt]'
	
	  print,'infile    - This is the filename which contains a list of fitted wham fits files'
	
	  print,'          -   the list should be for a single calibrator and from a single night.'
	
	  print,'date      - The date of the data in the infile (Use format YYMMDD)'
	
	  print,'calib     - The calibrator name for the data in the infile (no spaces; e.g. l_ori)'
	
	  print,'/sum_file - Print out parameters to summary file; filename=calib.date.trans.txt'
	
	  print,'/mkplt    - send plots to hardcopy; filename=calib.date.trans.ps'
	
	  print,'tune      - include tune on plots and sum_file; tune typically red, blue, center or all'
	
	  return
	
	endif

	
	if keyword_set(sum_file) then begin
	
	  sum_file=calib+'.'+date+'.trans.txt'
	
	  close,7
	
	  openw,7,sum_file
	
	endif
	
	
	
	if keyword_set(mkplt) then mkplt=1 else mkplt=0
	
	;mkplt=1  ; plot to file  
	
	;mkplt=0  ; plot to screen
	
	
	
	; Set some typical colors
	
	red=[255,250]
	
	green=[255255,150]
	
	yellow=[255255255,200]
	
	blue=[888111111,50]
	
	
	
	if mkplt eq 1 then begin
	
	 loadct,13
	
	 set_plot,'ps'
	
	 pltname=calib+'.'+date+'.trans.ps'
	
	 device,/inches,xsize=6.0,ysize=6.0,filename=pltname,xoffset=.25,yoffset=.25,/color
	
	endif
	
	
	
	;Typical SSS calibrators
	
	;calibs=['L-Ori','Zeta-Oph','Spica-HII','G194','G300']
	
	;ncalibs=n_elements(calibs)
	
	
	
	xtit='AIRMASS'
	
	ytit='LN(AREA)'
	
	
	
	readcol,infile,format='a',input_files
	
	n_inputs=n_elements(input_files)
	
	;!P.MULTI=[0,1,n_inputs]
	
	title=calib+date
	
	airmass=fltarr(n_inputs) ; airmass
	
	area=fltarr(n_inputs)  ; integrated intensity
	
	area_sd=fltarr(n_inputs) ; uncertainty of intensity
	
	pressa=fltarr(n_inputs) 

	pressb=fltarr(n_inputs) 
	
	
	for i=0,n_inputs-1 do begin
	
	 file=input_files[i]
	
	 hdr0=headfits(file,exten=0,/silent)
	
	 airm=sxpar(hdr0,'AIRMASS',/silent)
	
	 hdr=headfits(file,exten=3,errmsg=errmsg,/silent)

	 pressa[i]=sxpar(hdr0,'PAMON',/silent)
	 pressb[i]=sxpar(hdr0,'PBMON',/silent)
	
	 if errmsg ne '' then begin
	
	    print,'Fits file has no 3rd extension which would contain fit.'
	
	 endif else begin
	
	; Ignore results if Airmass is really really high and needs to be greater than 0
	
	  if airm lt 20. and airm gt 0. then airmass[i]= airm else airmass[i]=-9.
	
	  if airmass[i] gt 0 then begin
	
	
	
	;   Start with the geocoronal error in error calculation
	
	;   Actually only a problem when Geocoronal is blended with source.
	
	;   param='AREASD1'
	
	;   area_sd_i=sxpar(hdr,param,/silent)
	
	;   area_sd[i]=area_sd[i]+area_sd_i^2
	
	
	
	;   Add up the non-geocoronal areas, and uncertainties
	
	;   Assume there are not more than 9 Gaussian fits (is that reasonable?)
	
	;   and that Gaussian 1 is always the Geocoronal
	
	
	
	    for j=2,9 do begin
	
	      param='AREA'+string(j,format='$(i1)')
	
	      area_i=sxpar(hdr,param,/silent)
	
	      area[i]=area[i]+area_i
	
	      param='AREASD'+string(j,format='$(i1)')
	
	      area_sd_i=sxpar(hdr,param,/silent)
	
	      area_sd[i]=area_sd[i]+area_sd_i^2   ; add uncertainties in quadrature
	
	    endfor
	
	;   print,area[i]
	
	;   print,area_sd[i]
	
	  endif
	
	 endelse
	
	endfor

	area_sd=sqrt(area_sd)  ; after all uncertainty added in quad, take sqrt
	
	ln_sd=(area_sd/area+1.0) ; the uncertainy 
	
	ln_area=(area)           ; the intensity
	
	a=where(area gt 0,na)  ; find valid data

	;Find first tune value here

	tuneA=pressA[0]

	tunenum=0

	for i=1,n_inputs-1 do begin ; check the tunes here

	 	numgood=where(tuneA gt pressa[i]-2 and tuneA lt pressa[i]+2)

		if numgood lt 0 then begin

			tuneA=[[tuneA],[pressA[i]]]

			tunenum=tunenum+1

		endif
		
	;print, tuneA
	;print, 'hi'
	;print, tunenum

	endfor


	; Here I want to get the positions of all of the pressures with one

	pressapos=[0]

	uncerthold=[]
	scalehold=[]

;	for i=0,n_elements(tuneA)-1 do begin ; check the tunes here
;
;
;		if n_elements(pressapos) gt 1 then begin
;
;	 		hold= [[pressapos],[where (pressA gt tuneA[i]-2 and pressA lt tuneA[i]+2)]]
;	 		pressapos=hold
;	 	endif
;
;		if n_elements(pressapos) eq 1 then begin
;
;	 		pressapos= [where (pressA gt tuneA[i]-2 and pressA lt tuneA[i]+2)]
;
;	 	endif 

	 
		
;	endfor

	;pause 
	for i=0, n_elements(tunea)-1 do begin

		pressapos=where(abs(pressa-tunea[i]) le 2)

		;PLEASE WORK
		;a_diff=pressa[pressapos]
		a_diff=pressapos

		;print, pressapos

		if na gt 1 then begin

		 if n_elements(a_diff) GT 1 then begin


		 ;print,'airmass[a_diff]', airmass[a_diff], 'ln_area[a_diff]', ln_area[a_diff], 'a_diff', a_diff
		 lin=poly_fit(airmass[a_diff],ln_area[a_diff],1,chisq=chisq,covar=covar,measure_errors=ln_sd[a_diff],sigma=sigma,status=status,yband=yband,yerror=yerror,yfit=yfit)
		 slop=lin[1]
			
		 dslop=sigma[1] ; slope error?
			
		 slop_str='Slope = '+string(slop)+' +/- '+string(dslop)
			
		 yintcept=lin[0]
			
		 dyintcept=sigma[0] ;error in intercept?
			
		 yint_str='Ln(I_int)= '+string(yintcept)+' +/- '+string(dyintcept)
			
		 ymax=yintcept+.1
			
		 ymin=min(ln_area[a])-.1
			
		 xmin=0
			
		 xmax=max(airmass[a])+.5
		
		 ;so I want to use the yintercept and the slope.
		
		 intrinsic= yintcept
		
		 transmis=slop/intrinsic+yintcept/intrinsic
		 ;error
		 uncert=sqrt((dslop/slop)^2+(dyintcept/yintcept)^2)
		
		 scale=2-transmis
			
		 if ymax lt max(ln_area[a])+.1 then ymax = max(ln_area[a])+.1 ; in case neg slopes
			
		 label_ypos1=ymax-(ymax-ymin)*.1
			
		 label_ypos2=ymax-(ymax-ymin)*.15
			
		 label_xpos=xmax*.1
			
			
		 	if i EQ 0 then begin

		 		plot,airmass[a],ln_area[a],psym=4,xra=[xmin,xmax],/xs,yra=[ymin,ymax],/ys,xtitle=xtit,ytitle=ytit,title=title,charsize=1.2, charthick=2, thick=2

		 	endif else begin
		 		
		 		oplot,airmass[a],ln_area[a],psym=4,thick=2
		 	endelse
		 	
		 
		 if slop lt 0 then oplot,[xmin,xmax],[yintcept,yintcept+slop*xmax], thick=1
			
		 if slop ge 0 then oplot,[xmin,xmax],[yintcept,yintcept+slop*xmax], thick=1, color=red[mkplt],linestyle=2
			
		 oploterror,airmass[a],ln_area[a],ln_sd[a],psym=3
			
		 oplot,airmass[a],ln_area[a],psym=4,color=red[mkplt],thick=2
			
		 xyouts,label_xpos,label_ypos1,yint_str,charsize=1.2, charthick=2
			
		 xyouts,label_xpos,label_ypos2,slop_str,charsize=1.2, charthick=2

		 scalehold= [scalehold,scale]

		 uncerthold= [uncerthold,uncert]

		 endif else begin

		  if n_elements(a_diff) eq 1 then begin  ;  No line to plot set scale and uncertainty to 1 or 0
		
		    ymin=min(ln_area[a])-.1
		
		    ymax=max(ln_area[a])+.1
		
		    oplot,airmass,ln_area[a],psym=4
		
		    oploterror,airmass[a],ln_area[a],ln_sd[a],psym=3
		
		    oplot,airmass[a],ln_area[a],psym=4,color=red[mkplt], thick=2

		 	scalehold= [scalehold,1]

		 	uncerthold= [uncerthold,0]
		
		  endif
		
		endelse
			
		endif else begin
		
		  if na eq 1 then begin  ;  No line to plot
		
		    ymin=min(ln_area[a])-.1
		
		    ymax=max(ln_area[a])+.1
		
		    oplot,airmass,ln_area[a],psym=4,xra=[xmin,xmax],/xs,yra=[ymin,ymax],/ys,xtitle=xtit,ytitle=ytit,title=title, thick=2
		
		    oploterror,airmass[a],ln_area[a],ln_sd[a],psym=3
		
		    oplot,airmass[a],ln_area[a],psym=4,color=red[mkplt], thick=2

		    scalehold= 1

		 	uncerthold= 1
		
		  endif
		
		endelse

	endfor


; This checks if scale is bad. If using the calibrator results in a scale lt 1. then it resets scale to one and prints
; that the scale for this block is bad.

;print,'hi', scalehold, 'scalehold'

weighted=weight_ave(scalehold,uncerthold)

scale=weighted[0]
var=weighted[1] ; possibly need to square this, check!

if scale LT 1 then begin

	scale = 1

	print, date +' is a bad fit.'

endif


	
;	if mkplt eq 1 then begin
;	
;	 device,/close
;	
;	 set_plot,'x'
;	
;	 loadct,0
;	
;	endif
	
;	if keyword_set(sum_file) then begin
;	
;	  printf,7,'Calib_Src  Date   Tune   Ln(I_int) sigma  Slope   sigma   N   Airmass'
;	
;	  printf,7,'                                                     min   max'
;	
;	  printf,7,'--------- ------ ------- ------- ------- ------- ------- -- ----- -----
;	
;	  if na gt 1 then printf,7,format='$(a9,1x,1x,a7,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.5,1x,i2,1x,f5.2,1x,f5.2)',calib,date,yintcept,dyintcept,slop,dslop,na,min(airmass[a]),max(airmass[a])
;	
;	  if na eq 1 then printf,7,format='$(a9,1x,1x,a7,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.5,1x,i2,1x,f5.2,1x,f5.2)',calib,date,0.0,0.0,0.0,0.0,na,airmass[a],airmass[a]
;	
;	  close,7
;	
;	endif
return
end
