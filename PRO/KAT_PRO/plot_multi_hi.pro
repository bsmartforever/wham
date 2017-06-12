pro plot_multi_hi, glon, glat, vrange=vrange, norm=norm, radius=radius, save=save, names=names, hi_data=hi_data

; Purpose - Load and plot one or more LAB survey HI spectra.
;
;    glon - Galactic Longitude, either single number or an array.
;    glat - Galactic Latitude, either single number or an array.
;
;    [OPTIONAL]
;    vrange - [velocity min, velocity max]
;    norm - Vertically scale the HI spectra from 0 to 1.
;    radius - Set the radius (in degrees) of the circular region surrounding 
;            (glon,glat) to be included in the averaged HI spectra. 
;             DEFAULT: 0.55 degrees to match the WHAM beam size
;    save - Set output to postscript instead of to the terminal.
;	    DEFAULT: spectra.eps unless another name is passed, which includes '.eps'
;
; Created by Dr. Kat Barger February 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;        

;get and save current color table
tvlct, r, g, b, /get
;load white-black color table
cgLoadct,0,/reverse

colors=['black','blue','green','orchid','red','brown','orange']

;If more than 7 spectra, assign random colors.
if n_elements(glon) gt 7.0 then begin

   color_names= FSC_Color(/Names)
   numberOfNumbersNeeded = n_elements(glon)
   rng = Obj_New('RandomNumberGenerator', initialSeed) 
   randomNumbers = rng -> GetRandomNumbers(numberOfNumbersNeeded)
   color_index = fix(randomNumbers*(n_elements(fsc_color(color_names))-1.0),type=2)
   colors=color_names[color_index]

endif

!p.position=[0.15,0.125,0.95,0.95]
!p.charsize=1.5
!p.charthick=2
!x.thick=2
!y.thick=2
!p.thick=2

if keyword_set(save) then begin

    if size(save,/type) eq 2 then name="spectra.eps" $
    else if size(save,/type) eq 7 then name=save $ 
    else begin
         print,'*** error in file name convention, try again ***'
         set_plot,'x'
         save=0
         goto,advance
    endelse

    entry_device=!d.name
    set_plot,'PS'
    !p.font=0
    device,filename=name,bits_per_pixel=16,color=1
    device,/landscape,encapsulated=1, /helvetica
    device,xsize=10,xoffset=(11.0-10)*0.5,/inches

endif

advance:

if (NOT Keyword_Set(radius)) then radius=0.55
if (NOT Keyword_Set(hi_data)) then hi_data=0 

spectra=0
radius_original=radius
while size(spectra,/type) eq 2 do begin
      spectra=hi_spectra(glon,glat,radius=radius,hi_data=hi_data)
      radius=radius+0.01
endwhile
if radius_original ne radius then print,'radius: ',radius-0.01,format='(A-7,f-8.2)'

if keyword_set(vrange) then begin
   xmin=min(vrange) & xmax=max(vrange)   
   xgood=where((spectra[0].vel gt xmin) and (spectra[0].vel lt xmax))
end else begin
   xgood=indgen(n_elements(spectra[0].vel))
   xmin=min(spectra[0].vel) & xmax=max(spectra[0].vel)
endelse

if keyword_set(norm) then begin
   ;print,'got here'
   plot,spectra[0].vel[xgood],normal(spectra[0].data[xgood]),xrange=[xmin,xmax],xstyle=1,yrange=[0,1],$
        xtitle='LSR Velocity [km/s]',ytitle='Normalized Column Density',charsize=!p.charsize,color=fsc_color('black'),/nodata

   for i=0, n_elements(spectra)-1 do begin

       oplot,spectra[i].vel[xgood],normal(spectra[i].data[xgood]),color=fsc_color(colors[i])

   endfor
endif


if (NOT keyword_set(norm)) then begin
   ymin=-1.5e17 & ymax=max(spectra.data[xgood]*1.8224e18*1.1)
   plot,spectra[0].vel[xgood],spectra[0].data[xgood]*1.8224e18,xrange=[xmin,xmax],xstyle=1,$
        xtitle='LSR Velocity [km/s]',ytitle='Column Density',yrange=[ymin,ymax],ystyle=1,$
        color=fsc_color('black'),/nodata
   oplot,!x.crange,[0,0],linestyle=1   

   for i=0, n_elements(spectra)-1 do begin

       oplot,spectra[i].vel[xgood],spectra[i].data[xgood]*1.8224e18,color=fsc_color(colors[i])

   endfor
endif

yspace=0.035
for i=0, n_elements(spectra)-1 do begin
   if (NOT keyword_set(names)) then $
   legend,['('+strcompress(string(glon[i],format='(f8.2)')+', '+string(glat[i],format='(f8.2)'),/re)+')'],$
          linestyle=[0],position=[0.66,0.9-yspace*i],scale=0.5,box=0,/norm,color=[fsc_color(colors[i])], $ 
          TEXTCOLORS=fsc_color('black') $
   else begin 
      if (n_elements(names) eq n_elements(glon)) and (size(names,/type) eq 7) then legend,[names[i]],$
          linestyle=[0],position=[0.7,0.9-yspace*i],scale=0.5,box=0,/norm,color=[fsc_color(colors[i])], $ 
          TEXTCOLORS=fsc_color('black') 
   endelse
endfor


if keyword_set(save) then begin

    device,/close_file
    set_plot,entry_device
    Close, /All

endif

    set_plot,'x'

    !p.multi=0
    !p.thick=1
    !x.thick=1
    !y.thick=1

;pause

;restore original color table
tvlct, r, g, b

end