function intensity, spectrum=spectrum, $
   vel=vel, data=data, var=var, err=err, $
   boxcar=boxcar,bin=bin,$
   range=range, xrange=xrange,$
   bg=bg, savebg=savebg, order=order, click=click,$
   mR=mR, k2cm2=k2cm2, log=log, factor=factor,$
   Gauss=Gauss, ngauss=ngauss, mask=mask,$
   whamip=whamip, ip=ip, $
   fit_params=fit_params,$
   height=height,center=center,width=width,fwhm=fwhm,$
   fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
   tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
   filename=filename,date=date,hist=hist,$
   help=help, quiet=quiet,noerase=noerase

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Input:
;   Must call either spectrum or vel, and data with either var and err as optional.  
;   
;   spectrum - Structure containing vel, data, and var tags
;   vel      - Array containing velocities
;   data     - Array containing data 
;
;   [Optional]:
;   var        - Array containing (err)^2
;   err        - array containing error values
;   range      - Sets an integration velocity range
;   xrange     - Plotting range.
;   bg         - Sets a constant value for the baseline level. 
;                 If not set, then the user will be asked to specify.
;   savebg     - Save file to save the bg fit
;   order      - Maxiumum polynomial for continuum fitting
;   click      - Allows user to specify parameters by clicking on the plot instead 
;                 of inputting them manually. Parameters include 'bg','range', 
;                 and 'center' of Gaussians.
;   mR         - Specifies that the data array is in ADUs and to convert to mR.
;   hi         - Converts data array from Kelvin to cm^-2.
;   [Gauss Specific]:
;   Gauss      - Flag specifing Gaussian fitting as apposed to numarical integration.
;                 Default is to initialize fits with widths = 30 km/s.
;   ngauss     - Sets the number of Gaussians to fit. The 'Gauss' flag is 
;                 automatically if specified.              
;   whamip     - Calculate Gaussian fits using the WHAM intrument profile.
;   ip         - An array containing an instrument profile to be convolved with 
;                 Gaussian fit functions.
;   height     - Initial guess array for the Gaussian heights
;                 Default is (max(y)-min(y))/4.
;   center     - Initial guess array for the Gaussian centers.
;   width      - Initial guess array for the Gaussian width. Default is 30.0.
;   fix_height - FIX variable flag array for the Gaussian heights
;   fix_center - FIX variable flag array for the Gaussian centers
;   fix_width  - FIX variable flag array for the Gaussian width
;   tol_height - TOLERANCE array about initial gauss values for the Gaussian heights
;   tol_center - TOLERANCE array about initial gauss values for the Gaussian centers
;   tol_width  - TOLERANCE array about initial gauss values for the Gaussian width
;
; Output:
;   -Area and corresponding uncertanties.
;   -If Gaussian is set, then all Gaussian parameters are output and their 
;     corresponding uncertanties in a structure. Parameters included are 
;     the area, center, width, and height. The reduced Chi-Squared is printed
;     to command line.
;
; Examples: 
;             tmp=dirty_intensity(spectrum=hi_spectra,bg=0,xrange=[50,250],/gauss,/click)
;             tmp=dirty_intensity(vel=x,data=y,err=err,/click,/gauss,ngauss=2,bg=0,/wham)
;
; Created by Dr. Kat Barger
;
; Modifications:
;     -Now fits Gaussians using mpfit routines. This enhances the output statistics 
;      and overal fitting. Errors are no longer the 'Possion' noise and are now 
;      systematical uncertanties. The reduced Chi-Squared is now also calculated.
;     -Can now fit multiple Gaussians.  
;     -Can now convolve fits with instrument profiles. The 'whamip' keyword sets 
;      the WHAM IP. 
;     -Fixed the issue with each run of the program, a new window,1 is opened.
;      This enables the user to move the plotting window and to not have it moved
;      back every time the program is called.
;     -Fixed the selecting the left and right background part of the code, where
;      the user can select either the left or right limit first instead of just the
;      left, then the right limit.
;     -Fixed the replot that occurs when the plot is 'cleared'. The plot now correctly
;      resets to it's original version with error bars and not just an ugly plot,vel,data.
;     -Changed the psym=symcat(16) to psym=10 for a histogram plot look istead of a scatter plot.  
;
;    Restrictions:
;       -Assumes a flat and baselines
;
; To Do:
;     -Test initialize and constrain all Gaussian parameters keywords. 
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;get and save current color table
tvlct, r, g, b, /get
;load white-black color table
cgLoadct,0,/reverse,window=1
!p.background=0
!p.color=255
;!p.color=fsc_color('black')
;get current plotting window index
window_index=!d.window

    !p.multi=0
    !p.thick=3
    !x.thick=3
    !y.thick=3

if keyword_set(help) then begin
   print,''
   print,'function dirty_intensity, spectrum=spectrum,'
   print,'   vel=vel, data=data, var=var, err=err, '
   print,'   bg=bg, range=range, xrange=xrange,'
   print,'   click=click, mR=mR, hi=hi,'
   print,'   Gauss=Gauss, ngauss=ngauss, mask=mask,'
   print,'   whamip=whamip, ip=ip, quiet=quiet'
   print,''
   return,' ';
endif

if (size(filename,/type) eq 7) AND (NOT keyword_set(date)) then file=validate_extension(filename,'sav') $
  else if (size(filename,/type) eq 7) AND (keyword_set(date)) then file=validate_extension(filename+'_'+date_time(/time),'sav') $
  else if size(filename,/type) eq 2 then file='intensity_'+date_time(/time)+'.sav' 
  

if keyword_set(whamip) OR keyword_set(ip) then Gauss=1
if (NOT keyword_set(ngauss)) then ngauss=1

if (keyword_set(err)) then var=(err)^2.0
if (keyword_set(vel) and (NOT keyword_set(var))) then var=data*0.0
if (keyword_set(vel) and (NOT keyword_set(data))) or $
   (keyword_set(data) and (NOT keyword_set(vel))) then begin
   print,'*** Must specify vel and data ***'
   return,0;
endif

if keyword_set(k2cm2) then factor=!k2cm2 else $
if keyword_set(mR) then factor=1.0/22.8*1.0e3 else $
if (NOT keyword_set(factor)) then factor=1
if (NOT keyword_set(spectrum)) then spectrum=spectra_str(vel,data,var=var)
if (NOT keyword_set(boxcar)) then boxcar=0 else if boxcar eq 1. then boxcar=7.
spectrum.data=smooth(spectrum.data,boxcar) 
if (NOT keyword_set(bin)) then bin=1. else if bin eq 1. then bin=2.
length=round(n_elements(spectrum.vel)/float(bin))
spectrum={vel:frebin(spectrum.vel,length),data:frebin(spectrum.data,length),var:(frebin_err(sqrt(spectrum.var),length))^2.}

if (NOT keyword_set(xrange)) then xrange=[min(spectrum.vel),max(spectrum.vel)] $
   else begin
   if n_elements(xrange) ne 2.0 then begin
      xrange=[min(spectrum.vel),max(spectrum.vel)] 
      print,'*** Need to supply xrange as [vmin,vmax]! ***'
      print,'*** Calculating across the entire range! ***'
   endif
   xrange=[min(xrange),max(xrange)]
endelse

;This is to allow user input of bg=0
if n_elements(bg) eq 0 then calc_bg=1 else ycon=fltarr(n_elements(spectrum.vel))+bg[0]

if (NOT keyword_set(click)) and (keyword_set(calc_bg)) then begin
   print,'*** Must specify either CLICK or BG! ***'
   print,'        *** Assuming BG=0 ***' 
   calc_bg=0
endif

if (NOT keyword_set(range)) and (NOT keyword_set(click)) then begin
   print,'*** Must specify either range or click. ***'
   print,'*** Calculating the stdev over the entire range ***'
   range=[min(spectrum.vel),max(spectrum.vel)]
endif 

if n_elements(spectrum) gt 1 then begin
   print,'*** Can only calculate the intensity for one spectrum at a time! ***'
   print,'             *** Only calculating for the index = 0. ***'
   spectrum=spectrum[0]
endif

if keyword_set(range) and (NOT keyword_set(calc_bg)) then begin

   if n_elements(range) ne 2.0 then begin
      print,'*** Invalid range, please specify in the form [vmin, vmax] ***'
      return,0;
   endif

   temp=range
   range[0]=min(temp) & range[1]=max(temp)

   good=where((spectrum.vel ge range[0]) and (spectrum.vel le range[1]),count)
   if count eq 0 then begin
      print,'*** Invalid velocity range ***'
      print,'Please specify value between [vmin,vmax]: ',min(spectrum.vel),max(spectrum.vel)
      return,0;
   endif
   intensity=int_tabulated(spectrum.vel[good],spectrum.data[good]-bg,sort=1)
   intensity_err=sqrt(int_tabulated(spectrum.vel[good],spectrum.var[good],sort=1))

endif

if keyword_set(click) then begin
   ;Test to see if window 1 is open and open if it is not.
   ;By not opening it each time the program is ran, the user
   ;can move where they want the plotting window to be without
   ;it resetting on them each time.
   Device, Window_State=theseWindows
   if theseWindows[1] eq 0 then window,1
   wset,1

   if keyword_set(spectrum.var) then error=sqrt(spectrum.var) else error=spectrum.vel*0

   replot:

   if keyword_set(hist) then psym=10 else psym=symcat(16)
   ploterror,spectrum.vel,spectrum.data,$
        spectrum.vel*0.0,error,$
        psym=psym,symsize=1.5,$
        xtitle='Velocity [km/s]',$
        ytitle='Intensity',$
        xrange=xrange,xstyle=1,ystyle=1,$
        /nohat,errcolor=fsc_color('blk6'),$
         errthick=3,noerase=noerase,$
         yminor=1,charthick=1

   if keyword_set(calc_bg) then begin
      IF (NOT keyword_set(order)) then maxord=10. else maxord=order
      MYCONT,spectrum.vel,spectrum.data,xarray,yarray,store,1
      bg_xarr=xarray & bg_yarr=xarray
      MYFIT,spectrum.vel,spectrum.data,xarray,yarray,store,ycon,a,sigma,ycon_sig,2,maxord=maxord
      int_xarr=xarray & int_yarr=yarray

      if n_elements(ycon) eq 0 then GOTO, replot

      oplot,spectrum.vel,ycon,linestyle=1
      if keyword_set(savebg) then save,ycon,filename=validate_extension(savebg,'sav')
      ;use restore_var to save the ycon array in any varaible name
   endif
   if keyword_set(range) eq 0 then begin
      oplot,spectrum.vel,ycon,linestyle=1
      print,'Specify the mask over which to calculate the intensity'
      print,'Lower bound [CLICK]: '
      cursor,x1,y1,/data,/up
      oplot,[1.,1.]*x1,!y.crange,linestyle=1
      cursor,x2,y2,/data,/up
      print,'UPPER bound [CLICK]: '
      oplot,[1.,1.]*x2,!y.crange,linestyle=1   
      range=[min([x1,x2]),max([x1,x2])]
      if (NOT keyword_set(quiet)) then print,'[xmin, xmax]: ',range,format='(A-15,f-8.2,f-8.2)'   
   endif else range=!x.crange
   int_range=range

   good=where((spectrum.vel ge range[0]) and (spectrum.vel le range[1]))
   intensity=int_tabulated(spectrum.vel[good],spectrum.data[good],sort=1)
   intensity_err=sqrt(int_tabulated(spectrum.vel[good],spectrum.var[good],sort=1))

   if keyword_set(Gauss) then begin

      if (ngauss gt 1) AND (NOT keyword_set(center)) then begin
        center=fltarr(ngauss)
        for i=0,ngauss-1 do begin
            print,'Specify the center of the first Gaussian'
            print,'Center position [CLICK]: '
            cursor,x1,y1,/data,/up
            center[i]=x1
            oplot,[1.,1.]*x1,!y.crange,linestyle=1
        endfor
      endif

      c_fwhm=(2.*SQRT(2.*ALOG(2.)))
      if (keyword_set(fwhm)) then width=fwhm/(2.*SQRT(2.*ALOG(2.)))
      if (keyword_set(hi)) AND (NOT keyword_set(width)) then width=fltarr(ngauss)+10.

      xvar=spectrum.vel[good]
      yvar=spectrum.data[good]
      if (n_elements(spectrum.var) NE 0) then errval=sqrt(spectrum.var[good]) 
        fit=multi_gauss(xvar,yvar-ycon[good],err=errval[good],ngauss=ngauss,$
              height=height,center=center,width=width,$
              fix_height=fix_height,fix_center=fix_center,fix_width=fix_width,$
              tol_height=tol_height,tol_center=tol_center,tol_width=tol_width,$
              fit_params=fit_params,unconvol_params=unconvol_params,$
              wham=whamip,ip=ip,/quiet)

      if keyword_set(errval) then reduced_chisq=fit.chisq else reduced_chisq=fltarr(ngauss)

      for i=0,n_elements(fit_params)-1 do $

        if ngauss gt 1 then begin
          if (NOT keyword_set(whamip)) then begin
            ;Does not correct for instrument profile
            gauss_func = gaussian(fit.x,[fit_params[i].height,fit_params[i].center,$
              fit_params[i].width])+ycon[good]
            oplot,fit.x,gauss_func,color=fsc_color('Dodger Blue'),linestyle=2 
          endif else begin
            gauss_func = gaussian(fit.x,[unconvol_params[i].height,unconvol_params[i].center,$
              unconvol_params[i].width])+ycon[good]
            oplot,fit.x,mk_wham_gauss(fit.x,y_gauss=gauss_func),color=fsc_color('Dodger Blue'),linestyle=2 
          endelse 
        endif

      ;Does correct for instrument profile
      oplot,fit.x,fit.y+ycon[good],color=fsc_color('Dodger Blue')

      ;sort fit parameters by center positions
      order=sort(fit_params.center) 
      fit_params=fit_params[order]

      if (NOT keyword_set(quiet)) then begin
          print,''
          print,'** FIT Results:'
          print,'** Reduced Chi-Squared: ',reduced_chisq,format='(A-25,f-8.2)'
          if keyword_set(hi) then begin
              print,'Column','vcen','width','FWHM','height',format='(A-23,4(A-20))'
              format='((e-10.2,A-3,e-10.2),3(f-7.2,A-3,f-10.2),(f-7.4,A-3,f-10.4))'  
          endif else if keyword_set(mR) then begin
             print,'area','vcen','width','FWHM','height',format='(5(A-20))'
             format='(4(f-7.2,A-3,f-10.2),(f-7.4,A-3,f-10.4))' 
          endif else begin       
             print,'area','vcen','width','FWHM','height',format='(5(A-20))'
             format='(4(f-7.2,A-3,f-10.2),(f-7.4,A-3,f-10.4))' 
           endelse
           
           if (NOT keyword_set(log)) then begin 
              area=fit_params.area*factor 
              area_err=fit_params.area_err*factor
            for i=0, ngauss-1 do $
              print,fit_params[i].area*factor,'+/-',fit_params[i].area_err*factor,$
                 fit_params[i].center,'+/-',fit_params[i].center_err,$
                 fit_params[i].width,'+/-',fit_params[i].width_err,$
                 fit_params[i].width*c_FWHM,'+/-',fit_params[i].width_err*c_FWHM,$
                 fit_params[i].height,'+/-',fit_params[i].height_err,$
                 format=format  
            endif else begin
              area=alog10(fit_params.area*factor)  
              area_err=log_err(fit_params.area*factor,fit_params.area_err*factor)
              format='(5(f-7.2,A-3,f-10.2))' 
              for i=0, ngauss-1 do $
                print,area[i],'+/-',area_err[i],$
                   fit_params[i].center,'+/-',fit_params[i].center_err,$
                   fit_params[i].width,'+/-',fit_params[i].width_err,$
                   fit_params[i].width*c_FWHM,'+/-',fit_params[i].width_err*c_FWHM,$
                   fit_params[i].height,'+/-',fit_params[i].height_err,$
                   format=format 
            endelse 
      endif

   endif
   if (window_index ne -1) then wset,window_index
endif

    !p.multi=0
    !p.thick=1
    !x.thick=1
    !y.thick=1

;restore original color table
tvlct, r, g, b

   if (NOT keyword_set(quiet)) AND (NOT keyword_set(gauss)) then begin
      print,''
      print,'** Intensity ',format='(A-15)'
      print,intensity*factor,'+/-',intensity_err*factor,format='(f-8.3,A-4,f-8.3)'
      print,''
   endif

if (NOT keyword_set(fit_params)) then return,[intensity,intensity_err]*factor

fit_params_orig=fit_params

add_tag,fit_params,'intensity',0.0,fit_params
add_tag,fit_params,'intensity_err',0.0,fit_params
add_tag,fit_params,'vel',spectrum.vel,fit_params
add_tag,fit_params,'data',spectrum.data,fit_params
add_tag,fit_params,'var',spectrum.var,fit_params

add_tag,fit_params,'ycon',ycon,fit_params
add_tag,fit_params,'chisq',reduced_chisq,fit_params
add_tag,fit_params,'bg_xarr',bg_xarr,fit_params
add_tag,fit_params,'bg_yarr',bg_yarr,fit_params
add_tag,fit_params,'int_range',int_range,fit_params

for i=0,ngauss-1 do begin
  fit_params[i].area=fit_params_orig[i].area
  fit_params[i].area_err=fit_params_orig[i].area_err
  fit_params[i].center=fit_params_orig[i].center
  fit_params[i].center_err=fit_params_orig[i].center_err
  fit_params[i].height=fit_params_orig[i].height
  fit_params[i].height_err=fit_params_orig[i].height_err
  fit_params[i].intensity=area[i]
  fit_params[i].intensity_err=area_err[i]
endfor

if size(file,/type) eq 7 then begin 
  print,''
  print,'Structure contained within the saved file is named fit_params'
  save,fit_params,filename=file
endif

return,fit_params


end