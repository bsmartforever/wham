function three_sigma, spectrum=spectrum, vel=vel, data=data, var=var, $
  width=width, fwhm=fwhm,st_dev=st_dev, $
  range=range, xrange=xrange, click=click, mR=mR, hi=hi, k2cm2=k2cm2,$
  log=log

; Purpose - To determine three sigma upper limit of noise spectra. 
;
;    spectrum - A spectrum structure containing *.vel and *.data
;
;    [OR]
;
;    data - data array
;    vel - velocity array
;    var - [OPTIONAL] variance array 
;
;    MUST EITEHR SET SPECTRUM OR V AND D 
;
;    width - The width of the typical line. Used to determine the 
;	     three sigma area. Default width = 30 km/s. 
;	     NOTE: In the absence of a good width estimate, 
;	     use the HI width as a minimum; warm-ionized gas
;            will be broader as the temperature is higher.
;    fwhm - 
;
;    [OPTIONAL] - Must specify one, else calculate stdev across 
;		  the entire velocity range.
;    st_dev - Pass the standard deviation; do not calculate it.
;    range - Velocity range to calculate the standard deviation.
;    xrange - Plotting range.
;    click - Plot the spectrum and have the user click on the 
;            region to calculate the standard deviation.
;            This will open a new window.
;    mR - Convert to units of mR by multiplying 3sigma by 1.0/22.8*1.0e3
;
; By Dr. Kat Barger March 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;get and save current color table
tvlct, r, g, b, /get
;load white-black color table
cgLoadct,0,/reverse

;get current plotting window index
window_index=!d.window

    !p.multi=0
    !p.thick=3
    !x.thick=3
    !y.thick=3

if (keyword_set(vel) and (NOT keyword_set(var))) then var=data*0.0
if (keyword_set(vel) and (NOT keyword_set(data))) or $
   (keyword_set(data) and (NOT keyword_set(vel))) then begin
   print,'*** Must specify vel and data ***'
   return,0;
endif

if (NOT keyword_set(spectrum)) and (NOT keyword_set(st_dev)) then spectrum=spectra_str(vel,data,var=var)

if (NOT keyword_set(width)) then width = 30.
if (keyword_set(fwhm)) then width=fwhm/2.3548

if (NOT keyword_set(st_dev)) AND (NOT keyword_set(range)) AND (NOT keyword_set(click)) then begin
   print,'*** Must specify either st_dev, range, or click. ***'
   print,'*** Calculating the stdev over the entire range ***'
   range=[min(spectrum.vel),max(spectrum.vel)]
endif 

if n_elements(spectrum) gt 1 then begin
   print,'*** Can only calculates 3sigma lower limit for one spectrum at a time! ***'
   print,'             *** Only calculating for the index = 0. ***'
   spectrum=spectrum[0]
endif

if keyword_set(range) then begin
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
   st_dev=weight_stdev(spectrum.data[good],sqrt(spectrum.var[good]))
endif

if (NOT keyword_set(xrange)) then xrange=[min(spectrum.vel),max(spectrum.vel)] $
   else begin
   if n_elements(xrange) ne 2.0 then begin
      xrange=[min(spectrum.vel),max(spectrum.vel)] 
      print,'*** Need to supply xrange as [vmin,vmax]! ***'
      print,'*** Calculating across the entire range! ***'
   endif
   xrange=[min(xrange),max(xrange)]
endelse

if keyword_set(click) then begin
   window,1

   plot,spectrum.vel,spectrum.data,$
        xtitle='Velocity [km/s]',$
        ytitle='Intensity',/nodata,$
        xrange=xrange,xstyle=1

   if keyword_set(spectrum.var) then begin
      oploterror,spectrum.vel,spectrum.data,$
         spectrum.vel*0.0,sqrt(spectrum.var),$
         /nohat,errcolor=fsc_color('blk6'),$
         errthick=3,psym=symcat(16),symsize=2.0
   endif

   print,'Specify the region over which to calculate the stdev'
   print,'Lower bound [CLICK]: '
   cursor,x1,y1,/data,/up
   oplot,[1.,1.]*x1,!y.crange,linestyle=1
   cursor,x2,y2,/data,/up
   print,'UPPER bound [CLICK]: '
   oplot,[1.,1.]*x2,!y.crange,linestyle=1   
   range=[min([x1,x2]),max([x1,x2])]
   
   good=where((spectrum.vel ge range[0]) and (spectrum.vel le range[1]))
   st_dev=weight_stdev(spectrum.data[good],sqrt(spectrum.var[good]))

   if (window_index ne -1) then wset,window_index
endif

    !p.multi=0
    !p.thick=1
    !x.thick=1
    !y.thick=1

;restore original color table
tvlct, r, g, b

    if keyword_set(mR) then return, st_dev*3.0*width*1.0/22.8*1.0e3 $
      else if keyword_set(mR) AND keyword_set(log) then return, alog10(st_dev*3.0*width*1.0/22.8*1.0e3)
    if keyword_set(hi) OR keyword_set(k2cm2) AND keyword_set(log) then return, alog10(st_dev*3.0*width*1.8224e18) $
      else if (keyword_set(hi) OR keyword_set(k2cm2)) then return, st_dev*3.0*width*1.8224e18
    
    if (NOT keyword_set(log)) then return, st_dev*3.0*width else return, st_dev*3.0*width

end