

pro plotpreview,wave,flux,name

SET_PLOT, 'PS'
;
DEVICE, FILENAME='plotpreview.eps', FONT_SIZE=11, /INCHES, XSIZE=12, YSIZE=6, $
	/ENCAPSULATED, /COLOR, /portrait

!P.MULTI = [0, 1, 1]
!y.margin = [4,2]
!P.CHARSIZE = 2.1
!p.psym=10
loadct,40

wmin = min(wave)
wmax = max(wave)
gg = where(wave ge 900 and flux gt 0)
yavg = mean(flux[gg])
rflux = flux/yavg

yax = max(rflux[gg]) + 0.3*max(rflux[gg])

print, yax

	plot, wave, rflux, xr =[wmin,wmax],yr=[0,yax], nsum = 8, /xs, /ys, charthick = 2,$
	xtitle = '!6Wavelength (!n!sA!r!u!9 %!6 !N) ', ytitle = ' ', title = name;, $
	
xyouts,0.06,.5,'!6Relative Flux', orientation = 90, al = .5, charsize = 2.3, charthick = 2,/normal

DEVICE, /CLOSE


set_plot,'x'
cleanplot, /silent

END 
