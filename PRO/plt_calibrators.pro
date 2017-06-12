pro plt_calibrators, infile, date, calib, sum_file=sum_file, mkplt=mkplt, tune=tune, absflx=absflx, delflx=delflx,debug=debug, readstruct=readstruct

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
  print,'absflx    - if absflx is set, a second plot is make with a fit thru that absolute flx intercept.'
  print,'delflx    - if delflx is set, the ln(I) flux is adjusted by delflx before any fitting is done.'
  print,"          -  delflx is applied in cases where the flux calibrator's apparent flux has been degraded over time."
  print,"  Note: plt_calibrators routine requires mpfitexy.pro to be available/loaded before execution. "
  return
endif

if not keyword_set(tune) then tune='' else tune_str='_'+tune

if keyword_set(sum_file) then begin
  sum_file=calib+'.'+date+tune_str+'.trans.txt'
  if keyword_set(absflx) then sum_file=calib+'.'+date+tune_str+'.trans.abs.txt'
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
 pltname=calib+'.'+date+tune_str+'.trans.ps'
 if keyword_set(absflx) then pltname=calib+'.'+date+tune_str+'.trans.abs.ps'
 device,/inches,xsize=8.0,ysize=8.0,filename=pltname,xoffset=.25,yoffset=.25,/color
endif

;Typical SSS calibrators
;calibs=['L-Ori','Zeta-Oph','Spica-HII','G194','G300']
;ncalibs=n_elements(calibs)

xtit='AIRMASS'
ytit='LN(AREA)'

if NOT keyword_set(readstruct) then begin
readcol,infile,format='a',input_files
endif

if keyword_set(readstruct) then begin
input_files = infile
endif

n_inputs=n_elements(input_files)
;!P.MULTI=[0,1,n_inputs]
print, n_inputs
title=calib+'  '+tune+'  '+date
airmass=fltarr(n_inputs) ; airmass
area=fltarr(n_inputs)  ; integrated intensity
area_sd=fltarr(n_inputs) ; uncertainty of intensity
IP_Used=strarr(n_inputs) ; IP used
TuneA=fltarr(n_inputs) ; Tune A 
TuneB=fltarr(n_inputs) ; Tune B 
TuneDiff=fltarr(n_inputs) ; Tune diff

for i=0,n_inputs-1 do begin

if NOT keyword_set(readstruct) then begin
 file=input_files[i]
 hdr0=headfits(file,exten=0,/silent)
 airm=sxpar(hdr0,'AIRMASS',/silent)
 TnA=sxpar(hdr0,'PAMON',/silent)
 TnB=sxpar(hdr0,'PBMON',/silent)
 hdr=headfits(file,exten=3,errmsg=errmsg,/silent)
 IP=sxpar(hdr,'IP',/silent)
 IP_Used[i]=IP
 TuneA[i]=TnA
 TuneB[i]=TnB
 TuneDiff[i]=TnB-TnA
 endif

if keyword_set(readstruct) then begin
 file=input_files[i]
 airm=input_files[i].airmass
 print, airm
 TnA=input_files.pamon
 TnB=input_files.pbmon
 errmsg=''
 IP='IP'
 IP_Used[i]=IP
 TuneA[i]=TnA
 TuneB[i]=TnB
 TuneDiff[i]=TnB-TnA
  endif


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
if keyword_set(debug) then begin
  print,area,area_sd,airmass
  stop
endif

area_sd=sqrt(area_sd)  ; after all uncertainty added in quad, take sqrt
ln_sd=alog(area_sd/area+1.0) ; the uncertainy in natural log space
ln_area=alog(area)           ; the intensity in natural log space
;a=where(area gt 0,na)  ; find valid data
a=where(area gt 0 and airmass le 90,na)  ; find valid data, limit airmass to avoid eroneous data

dflx=0.0
if keyword_set(delflx) then dflx=delflx
ln_area=ln_area+dflx

if keyword_set(absflx) then begin
 I0=absflx
 abs_lin=mpfitexy(airmass[a],ln_area[a],0.0*ln_sd[a]+0.001,ln_sd[a],guess=[-0.07,absflx],/fixint,/quiet,errors=fiterrors)
 abs_slop=abs_lin[0]
 abs_slop_err=fiterrors[0]
endif

if na gt 1 then begin
; lin=linfit(airmass[a],alog(area[a])) ; no error weighting
  lin=poly_fit(airmass[a],ln_area[a],1,chisq=chisq,covar=covar,measure_errors=ln_sd[a],sigma=sigma,status=status,yband=yband,yerror=yerror,yfit=yfit)
  slop=lin[1]
  dslop=sigma[1]
  slop_str='Slope = '+string(slop,format='$(f7.4)')+' +/- '+string(dslop,format='$(f7.5)')
  yintcept=lin[0]
  dyintcept=sigma[0]
  yint_str='Ln(I_int)= '+string(yintcept,format='$(f6.3)')+' +/- '+string(dyintcept,format='$(f6.4)')
  ymax=yintcept+.1
  if keyword_set(absflx) then begin
    if ymax lt absflx then ymax=absflx+.1
    aslop_str='Slope = '+string(abs_slop,format='$(f7.4)')+' +/- '+string(abs_slop_err,format='$(f7.5)')
    ayint_str='Ln(I_0)= '+string(I0,format='$(f6.3)')
  endif
  ymin=min(ln_area[a])-.1
  xmin=0
  xmax=max(airmass[a])+.5
  if ymax lt max(ln_area[a])+.1 then ymax = max(ln_area[a])+.1 ; in case neg slopes
  label_ypos1=ymax-(ymax-ymin)*.1
  label_ypos2=ymax-(ymax-ymin)*.15
  label_ypos3=ymax-(ymax-ymin)*.20
  label_ypos4=ymax-(ymax-ymin)*.25
  label_xpos=xmax*.1
  label_xpos2=xmax*.2

  plot,airmass[a],ln_area[a],psym=4,xra=[xmin,xmax],/xs,yra=[ymin,ymax],/ys,xtitle=xtit,ytitle=ytit,title=title,charsize=1.2, charthick=2, thick=2
  if slop lt 0 then oplot,[xmin,xmax],[yintcept,yintcept+slop*xmax], thick=1
  if slop ge 0 then oplot,[xmin,xmax],[yintcept,yintcept+slop*xmax], thick=1, color=red[mkplt],linestyle=2
  oploterror,airmass[a],ln_area[a],ln_sd[a],psym=3
  oplot,airmass[a],ln_area[a],psym=4,color=red[mkplt],thick=2
  xyouts,label_xpos,label_ypos1,yint_str,charsize=1.2, charthick=2
  xyouts,label_xpos,label_ypos2,slop_str,charsize=1.2, charthick=2
  if keyword_set(absflx) then begin
   oplot,[xmin,xmax],[I0,I0+abs_slop*xmax], thick=1, color=red[mkplt],linestyle=0 
   xyouts,label_xpos2,label_ypos3,ayint_str,charsize=1.2, charthick=2, color=red[mkplt]
   xyouts,label_xpos2,label_ypos4,aslop_str,charsize=1.2, charthick=2, color=red[mkplt]
  endif
endif else begin
  if na eq 1 then begin  ;  No line to plot
    xmin=0
    xmax=max(airmass[a])+.5
    ymin=min(ln_area[a])-.1
    ymax=max(ln_area[a])+.1
    if ymax lt max(ln_area[a])+.1 then ymax = max(ln_area[a])+.1 ; in case neg slopes
    label_ypos1=ymax-(ymax-ymin)*.1
    label_ypos2=ymax-(ymax-ymin)*.15
    label_ypos3=ymax-(ymax-ymin)*.20
    label_ypos4=ymax-(ymax-ymin)*.25
    label_xpos=xmax*.1
    label_xpos2=xmax*.2
    if keyword_set(absflx) then begin
      if ymax lt absflx then ymax=absflx+.1
    endif
    if keyword_set(absflx) then begin
      if ymax lt absflx then ymax=absflx+.1
      aslop_str='Slope = '+string(abs_slop,format='$(f7.4)')+' +/- '+string(abs_slop_err,format='$(f7.5)')
      ayint_str='Ln(I_0)= '+string(I0,format='$(f6.3)')
    endif
    plot,airmass[a],ln_area[a],psym=4,xra=[xmin,xmax],/xs,yra=[ymin,ymax],/ys,xtitle=xtit,ytitle=ytit,title=title, thick=2
    oploterror,airmass[a],ln_area[a],ln_sd[a],psym=3
    oplot,airmass[a],ln_area[a],psym=4,color=red[mkplt], thick=2
    if keyword_set(absflx) then begin
     oplot,[xmin,xmax],[I0,I0+abs_slop*xmax], thick=1, color=red[mkplt],linestyle=0 
     xyouts,label_xpos2,label_ypos3,ayint_str,charsize=1.2, charthick=2, color=red[mkplt]
     xyouts,label_xpos2,label_ypos4,aslop_str,charsize=1.2, charthick=2, color=red[mkplt]
    endif
  endif
endelse
if mkplt eq 1 then begin
 device,/close
 set_plot,'x'
 loadct,0
endif
if keyword_set(sum_file) then begin
  if keyword_set(absflx) then begin
   printf,7,'Calib_Src  Date   Tune   Ln(I_int) sigma  Slope   sigma  Ln(I_0)  Slope   sigma   N   Airmass   PAMON  PBMON  Tune  IP                   Del_Ln(I) '
   printf,7,'                                                                                     min   max                Diff                        Applied  '
   printf,7,'--------- ------ ------- ------- ------- ------- ------- ------- ------- ------- -- ----- ----- ------ ------ ----- -------------------- ----------'
   if na gt 1 then printf,7,format='$(a9,1x,a6,1x,a7,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.4,1x,f7.5,1x,i2,1x,f5.2,1x,f5.2,1x,f6.1,1x,f6.1,1x,f5.1,1x,a20,1x,f10.7)',calib,date,tune,yintcept,dyintcept,slop,dslop,I0,abs_slop,abs_slop_err,na,min(airmass[a]),max(airmass[a]),mean(TuneA[a]),mean(TuneB[a]),mean(TuneDiff[a]),IP_Used[a[0]],dflx
   if na eq 1 then printf,7,format='$(a9,1x,a6,1x,a7,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.4,1x,f7.5,1x,i2,1x,f5.2,1x,f5.2,1x,f6.1,1x,f6.1,1x,f5.1,1x,a20,1x,f10.7)',calib,date,tune,0.0,0.0,0.0,0.0,I0,abs_slop,abs_slop_err,na,airmass[a],airmass[a],mean(TuneA[a]),mean(TuneB[a]),mean(TuneDiff[a]),IP_Used[a[0]],dflx
  endif else begin
   printf,7,'Calib_Src  Date   Tune   Ln(I_int) sigma  Slope   sigma   N   Airmass'
   printf,7,'                                                             min   max'
   printf,7,'--------- ------ ------- ------- ------- ------- ------- -- ----- -----
   if na gt 1 then printf,7,format='$(a9,1x,a6,1x,a7,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.5,1x,i2,1x,f5.2,1x,f5.2,1x,f6.1,1x,f6.1,1x,f5.1,1x,a20,1x,f10.7)',calib,date,tune,yintcept,dyintcept,slop,dslop,na,min(airmass[a]),max(airmass[a]),mean(TuneA[a]),mean(TuneB[a]),mean(TuneDiff[a]),IP_Used[a[0]],dflx
   if na eq 1 then printf,7,format='$(a9,1x,a6,1x,a7,1x,f7.4,1x,f7.5,1x,f7.4,1x,f7.5,1x,i2,1x,f5.2,1x,f5.2,1x,f6.1,1x,f6.1,1x,f5.1,1x,a20,1x,f10.7)',calib,date,tune,0.0,0.0,0.0,0.0,na,airmass[a],airmass[a],mean(TuneA[a]),mean(TuneB[a]),mean(TuneDiff[a]),IP_Used[a[0]],dflx
  endelse
  close,7
endif

end
