function wavelength, v, wavelength=wavelength, line=line, filter=filter, offset_vlsr=offset_vlsr, inverse=inverse, help=help, quiet=quiet,geo_to_lsr=geo_to_lsr,lsr_to_geo=lsr_to_geo,ps=ps

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Purpose: Calculates the wavelength of the emission given a rest 
;          wavelength and the velocity of the emission 
;
; v - The velocity of the emission in km/s. Required, unless /help.
;     Accepts both single values and arrays.
; wavelength - rest wavelength of the line in Angstroms. 
;              Default is H-alpha if otherwise unspecified.
; 
; keywords:
; offset_vlsr - offset offset_vlsr value from the geocentric rest frame. 
; inverse - Will calculate the velocity given supplied shifted wavelength. 
;           With this keyword set, the function takes the form:
;           temp=wavelength(shifted_wavelength)
; offset_vlsr = The velocity offset from the Geocentric Rest Frame
; geo_to_lsr = convert from Geocentric Rest Frame to Local Standard of Rest
; lsr_to_geo = convert from Local Standard of Rest Frame to Geocentric Rest Frame
; 
; filter - keyword to plot the filter transmission curves overlaid
;          with the position of the rest and shifted wavelengths.
; help - prints the call procedure and the rest wavelengths for 
;        WHAM lines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

original=v

if keyword_set(help) then begin
   print,''
   print,'final_wavelength=wavelength(center_velocity,wavelength=rest_wavelength,filter=filter,help=help)'
   print,'H-alpha is the default rest wavelength'
   print,' '
   print,'Rest wavelengths [Angstroms]:'
   print,'H-beta: 4861.3'
   print,'[OIII]: 5006.9'
   print,'HeI: 5875.6'
   print,'[NII]_blue: 5754.6'
   print,'[OI]: 6300.3'
   print,'H-alpha: 6562.8'
   print,'[NII]: 6583.4'
   print,'[SII]: 6716.4'
   print,'[OII]: 7320.0'

   return,0;
endif

IF keyword_set(line) THEN BEGIN
	CASE line OF
		'hb': BEGIN
			lambda=4861.3 &  line='Hb'
		END
		'oiii': BEGIN
			lambda=5006.9 &  line='[O III]'
		END
		'blue_nii': BEGIN
			lambda=5754.6 & line='blue [N II]'
		END
		'hei': BEGIN
			lambda=5875.6 &  line='He I'
		END
		'oi': BEGIN
			lambda=6300.3 &  line='[O I]'
		END
		'ha': BEGIN
			lambda=6562.8 & line='Ha'
		END
		'nii': BEGIN
			lambda=6583.4 & line='[N II]'
		END
		'sii': BEGIN
			lambda=6716.4 & line='[S II]'
		END
		'oii': BEGIN
			lambda=7320.0 &  line='[O II]'
		END
	ELSE: message, 'Unknown line ', line, ' Assuming H-alpha'
	ENDCASE
    wavelength=lambda
ENDIF

if (NOT keyword_set(offset_vlsr)) then offset_vlsr=0.0


v=float(v)

;converts to an array
vtemp=fltarr(n_elements(v))
vtemp=v
v=vtemp


c=299792.458 ;km/s
H_alpha=6562.8 ;Angstroms

bad=where(v eq 0,count)
If count ne 0 then v[bad]=0.00001
If NOT keyword_set(wavelength) then wavelength=H_alpha

if keyword_set(inverse) then begin
   
    shift_wavelength=v

    v=c-c*wavelength/shift_wavelength
 
    ;offset_vlsr = vgeo - offset_vlsr
    if keyword_set(offset_vlsr) and keyword_set(geo_to_lsr) then v=v-offset_vlsr
    if keyword_set(offset_vlsr) and keyword_set(lsr_to_geo) then v=v+offset_vlsr
 
   temp=v
   v=original
   return,double(temp);
end


if keyword_set(offset_vlsr) and keyword_set(geo_to_lsr) then v=v-offset_vlsr
if keyword_set(offset_vlsr) and keyword_set(lsr_to_geo) then v=v+offset_vlsr

shift_wavelength=wavelength*(c/v)/((c/v)-1)
if NOT keyword_set(quiet) then print,'Shifted wavelength: ',shift_wavelength

if keyword_set(filter) then begin
;To use, change the the directory below to the location of the filter transmission curves.

dir='$HOME/PRO/KAT_PRO/filters/'

!p.background=fsc_color('white')
!p.color=fsc_color('black')

!p.thick=3
!x.thick=3
!y.thick=3
!p.charsize=1.2


if keyword_set(ps) then begin
   filter=1 ;Turn plotting on.

    if size(ps,/type) eq 7 then name=ps else name="filter.eps"

    

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

endif




readcol,'$HOME/WHAM/Filters/Fe.dat',wave,trans,/silent
plot,wave,trans,xrange=[shift_wavelength-100,shift_wavelength+100],yrange=[0,1],xstyle=1,ystyle=1,/nodata,$
     xtitle='Wavelength [Angstroms]',ytitle='Transmission',color=fsc_color('black')
oplot,wave,trans,linestyle=0,color=FSC_COLOR("Grey", !D.Table_Size-4)
oplot,[1.,1.]*shift_wavelength,!y.crange,linestyle=2,thick=4,color=fsc_color('black')

readcol,dir+'H_Bar.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("RED6", !D.Table_Size-4)

readcol,dir+'H_Omega.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("GRN5", !D.Table_Size-4)

readcol,dir+'HeI.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Olive", !D.Table_Size-4)

readcol,dir+'NII_red.dat',wave,trans,/silent
oplot,wave,trans,linestyle=0,color=FSC_COLOR("RED3", !D.Table_Size-4)

readcol,dir+'OI.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("BLU4", !D.Table_Size-4)

readcol,dir+'OIII.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Rosy Brown", !D.Table_Size-4)

readcol,dir+'H2O.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Goldenrod", !D.Table_Size-4)

readcol,dir+'H_Blue.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("BLU7", !D.Table_Size-4)

readcol,dir+'H_beta.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Orange Red", !D.Table_Size-4)

readcol,dir+'NII_Blue.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Deep Pink", !D.Table_Size-4)

readcol,dir+'Na.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Lime Green", !D.Table_Size-4)

readcol,dir+'OII.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Charcoal", !D.Table_Size-4)

readcol,dir+'SII.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Dark Orchid", !D.Table_Size-4)

readcol,dir+'Filter_5202.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("slate blue", !D.Table_Size-4)

readcol,dir+'Filter_6577.dat',wave,trans,/silent
oplot,wave,trans,color=FSC_COLOR("Pink", !D.Table_Size-4)

yoff=0.007
xyouts,.78,.88,'HB',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.945,0.945]-yoff,linestyle=0,color=FSC_COLOR("Orange Red", !D.Table_Size-4)
xyouts,.78,.85,'OIII',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.91-0.002,0.91-0.002]-yoff,linestyle=0,color=FSC_COLOR("Rosy Brown", !D.Table_Size-4)
xyouts,.78,.82,'NII-Blue',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.875-0.002*2.0,0.875-0.002*2.0]-yoff,linestyle=0,color=FSC_COLOR("Deep Pink", !D.Table_Size-4)
xyouts,.78,.79,'HeI',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.840-0.002*3.0,0.840-0.002*3.0]-yoff,linestyle=0,color=FSC_COLOR("Olive", !D.Table_Size-4)
xyouts,.78,.76,'Na',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.805-0.002*4.0,0.805-0.002*4.0]-yoff,linestyle=0,color=FSC_COLOR("Lime Green", !D.Table_Size-4)
xyouts,.78,.73,'H2O+',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.770-0.002*5.0,0.770-0.002*5.0]-yoff,linestyle=0,color=FSC_COLOR("Goldenrod", !D.Table_Size-4)
xyouts,.78,.70,'OI',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.735-0.002*5.0,0.735-0.002*5.0]-yoff,linestyle=0,color=FSC_COLOR("BLU4", !D.Table_Size-4)
xyouts,.78,.67,'Fe',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.700-0.002*6.0,0.700-0.002*6.0]-yoff,linestyle=0,color=FSC_COLOR("Grey", !D.Table_Size-4)
xyouts,.78,.64,'Ha-Blue',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.665-0.002*7.0,0.665-0.002*7.0]-yoff,linestyle=0,color=FSC_COLOR("BLU7", !D.Table_Size-4)
xyouts,.78,.61,'Ha-Omega',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.630-0.002*8.0,0.630-0.002*8.0]-yoff,linestyle=0,color=FSC_COLOR("GRN5", !D.Table_Size-4)
xyouts,.78,.58,'Ha-Bar',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.595-0.002*9.0,0.595-0.002*9.0]-yoff,linestyle=0,color=FSC_COLOR("RED6", !D.Table_Size-4)
xyouts,.78,.55,'NII-Red',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.560-0.002*10,0.560-0.002*10]-yoff,linestyle=0,color=FSC_COLOR("RED3", !D.Table_Size-4)
xyouts,.78,.52,'SII',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.525-0.002*11.0,0.525-0.002*11.0]-yoff,linestyle=0,color=FSC_COLOR("Dark Orchid", !D.Table_Size-4)
xyouts,.78,.49,'OII',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.490-0.002*12.0,0.490-0.002*12.0]-yoff,linestyle=0,color=FSC_COLOR("Charcoal", !D.Table_Size-4)

xyouts,.78,.46,'H-Red',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.455-0.002*12.0,0.455-0.002*12.0]-yoff,linestyle=0,color=FSC_COLOR("pink", !D.Table_Size-4)

xyouts,.78,.43,'NI',/normal
oplot,(!x.crange[1]-!x.crange[0])*0.98*[0.92,.98]+!x.crange[0],[0.42-0.002*12.0,0.42-0.002*12.0]-yoff,linestyle=0,color=FSC_COLOR("slate blue", !D.Table_Size-4)

oplot,[1.,1.]*wavelength,[0,1],linestyle=1,thick=4

legend,['Rest','Shifted'],linestyle=[1,2],box=0,charsize=1.25,thick=4

endif

if keyword_set(ps) then begin

    !p.multi=0
    !p.thick=1
    !x.thick=1
    !y.thick=1
    !p.color=fsc_color('black')

    device,/close_file
    set_plot,entry_device
    Close, /All

    set_plot,'x'

endif

temp=shift_wavelength
v=original
return,double(temp);

end