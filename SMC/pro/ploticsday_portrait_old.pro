PRO ploticsday, logfile, title=title, smooth=smooth, $
	begintime=begintime, endtime=endtime

log = read_icslog(logfile)
IF keyword_set(begintime) THEN log = log[where(log.sysclock GE begintime)]
IF keyword_set(endtime) THEN log = log[where(log.sysclock LE endtime)]

xmarsave = !x.margin
!x.margin = [10, 30]
!p.multi = [0, 1, 4]
!x.style=1

multiplot
smooth=60

title = keyword_set(title) ? title : string(min(log.sysclock), format='(C())') + ' to ' + string(max(log.sysclock), format='(C())') 

plot, smooth(log.sysclock, smooth), smooth(log.outside_temp, smooth), ytitle='Temp (C)', yr=[-10, 40], ys=8, title = title
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='Temp (F)'
oplot, smooth(log.sysclock, smooth), smooth(log.siderostat_temp, smooth), linestyle=2
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_temp, smooth), linestyle=1
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_temp, smooth), linestyle=3

plots, [0.8, 0.85], [0.9, 0.9], linestyle=0, /normal
xyouts, 0.86, 0.895, 'Outside', /normal
plots, [0.8, 0.85], [0.87, 0.87], linestyle=2, /normal
xyouts, 0.86, 0.865, 'Siderostat', /normal
plots, [0.8, 0.85], [0.84, 0.84], linestyle=1, /normal
xyouts, 0.86, 0.835, 'Trailer', /normal
plots, [0.8, 0.85], [0.81, 0.81], linestyle=3, /normal
xyouts, 0.86, 0.805, 'Spectrometer', /normal

;plot, log.sysclock, log.heater, ys=4

multiplot

plot, smooth(log.sysclock, smooth), smooth(log.siderostat_temp, smooth), linestyle=2, ytitle='Inside Temp (C)', yr=[20, 30], ys=9
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='Inside Temp (F)'
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_temp, smooth), linestyle=1
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_temp, smooth), linestyle=3

multiplot

plot, smooth(log.sysclock, smooth), smooth(log.outside_humid, smooth), ytitle='% Relative Humidity', $
	yr=[0,110],/ys
oplot, smooth(log.sysclock, smooth), smooth(log.siderostat_humid, smooth), linestyle=2
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_humid, smooth), linestyle=1
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_humid, smooth), linestyle=3

multiplot

plot,log.sysclock,log.windspeed, psym=3, ytitle='Wind speed (mph)', yr=[0,15], $
	xtitle='time', xtickformat='(C(CHI2.2, ":", CMI2.2))'

plot, log.windspeed, (90-log.winddir)*!pi/180., /polar, psym=3,/iso,xs=5,ys=5, $
	xr=[-10, 10], yr=[-10, 10], $
	position= [ !p.position[2]+0.03, !p.position[1]+0.05, 0.95, !p.position[3] ]
axis, 0, 0, xaxis=0, /xs, xr=!x.crange
axis, 0, 0, yaxis=0, /ys
xyouts, -0.3, !y.crange[1]+0.3, 'N', charsize=2
xyouts, !x.crange[1]+0.3, -0.3, 'E', charsize=2
xyouts, -0.3, !y.crange[0]-1.9, 'S', charsize=2
xyouts, !x.crange[0]-1.9, -0.3, 'W', charsize=2

multiplot, /reset

IF !D.name EQ 'PS' THEN BEGIN ;;; plot second page
	!p.multi = [0, 1, 4]
	plot, [0], xr=[10, 20], xs=4, ys=4
	!p.multi = [0, 1, 4]
	multiplot
	
	plot, log.sysclock, log.ccdtemp, ytitle='CCD Temp (C)', yr=[-220, 30]
	
	multiplot, /reset
ENDIF

!p.multi = 0
!x.margin = xmarsave
!p.multi = 0
!p.psym=0
!x.style=0

END
