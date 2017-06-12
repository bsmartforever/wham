PRO ploticsday, logfile, title=title, smooth=smooth, $
	begintime=begintime, endtime=endtime

log = read_icslog(logfile)
IF keyword_set(begintime) THEN log = log[where(log.sysclock GE begintime)]
IF keyword_set(endtime) THEN log = log[where(log.sysclock LE endtime)]

postscript = !D.name EQ 'PS' ? 1 : 0
xmarsave = !x.margin & ymarsave = !y.margin
!x.margin = postscript ? [6, 65] : [10, 100]
!x.ticks=2 & !x.minor=11 & !x.ticklen=0.05
!y.margin = [10., 2.]
!x.style=1

multiplot, [1, 5], /rowmajor
smooth=keyword_set(smooth) ? smooth : 1

title = keyword_set(title) ? title : string(min(log.sysclock), format='(C())') + ' to ' + string(max(log.sysclock), format='(C())')

xyouts, 0.1, 0.98, 'ICS summary for ' + title, /normal, charsize=1.5

plot, smooth(log.sysclock, smooth), smooth(log.outside_temp, smooth), ytitle='Temp (C)', ys=8, yr=[-10, 40]
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='Temp (F)'
oplot, smooth(log.sysclock, smooth), smooth(log.siderostat_temp, smooth), linestyle=2
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_temp, smooth), linestyle=1
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_temp, smooth), linestyle=3

multiplot

plot, log.sysclock, log.heater, ytitle='I!Lheater!N (amp)'

multiplot

plot, smooth(log.sysclock, smooth), smooth(log.outside_humid, smooth), ytitle='% Rel humidity', $
	yr=[0,110],/ys
oplot, smooth(log.sysclock, smooth), smooth(log.siderostat_humid, smooth), linestyle=2
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_humid, smooth), linestyle=1
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_humid, smooth), linestyle=3

multiplot

plot, smooth(log.sysclock, smooth), smooth(log.baropres, smooth), $
	ytitle='Baro pres (mm Hg)', ys=16

multiplot

plot,log.sysclock,log.windspeed, psym=3, ytitle='Wind (mph)', $
	xtickformat='(C(CMOI, "/", CDI, " ", CHI2.2, ":", CMI2.2))'

multiplot, /reset

!x.margin = [!x.margin[1], !x.margin[0]]

multiplot, [1, 4], /rowmajor
	
plot, smooth(log.sysclock, smooth), smooth(log.siderostat_temp, smooth), linestyle=2, ytitle='Inside Temp (C)', ys=9, yr=[10, 25]
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='Inside Temp (F)'
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_temp, smooth), linestyle=1
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_temp, smooth), linestyle=3

multiplot

plot, smooth(log.sysclock, smooth), smooth(log.acin_temp, smooth), $
	ytitle='AC temp (C)', ys=8;, yr=[10, 30]
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='AC Temp (F)'
oplot, smooth(log.sysclock, smooth), smooth(log.acout_temp, smooth), linestyle=2

multiplot

plot, log.sysclock, log.ccdtemp, yr=[-120, 35], ytitle='CCD Temp (C)', /ys

multiplot

plot, log.sysclock, log.cha_pres, ytitle='Ch pres (mm Hg)', $
	xtickformat='(C(CMOI, "/", CDI, " ", CHI2.2, ":", CMI2.2))'
oplot, smooth(log.sysclock, smooth), smooth(log.chb_pres, smooth), linestyle=1

firsthour = where(log.sysclock LE log[0].sysclock + 0.5/24.)
lasthour = where(log.sysclock GE log[n_elements(log)-1].sysclock - 0.5/24)
xyouts, 0.05, 0.11, /normal, 'SF!L6!N mean weight: first 30 min:' + $
	string(mean(log[firsthour].sf6weight),format='(I4)')+' lb; last 30 min:' + $
	string(mean(log[lasthour].sf6weight), format='(I4)') + ' lb'

xyouts,0.05,0.08,/normal,'Cal mirror in beam ' + $
	strtrim(fix(total(log.cm_beam)),2) + ' / ' + strtrim(n_elements(log), 2) + $
	' minutes, stowed ' + strtrim(fix(total(log.cm_stow)),2)

xyouts,0.05,0.05,/normal,'LN!L2!N fill enable ' + $
	strtrim(fix(total(log.ln2_fill)),2) +' / ' + strtrim(n_elements(log), 2) + $
	' minutes'

xyouts,0.05,0.02,/normal,'Shutter open ' + $
	strtrim(fix(total(log.shutter)),2) +' / ' + strtrim(n_elements(log), 2) + $
	' minutes'

xyouts,0.36, 0.02, /normal,'Cal lamp on ' + $
	strtrim(fix(total([log.callamp_a, log.callamp_b, log.callamp_c, log.callamp_d])),2) + $
	' minutes'

xyouts, 0.9, 0.0, 'smooth=' + strtrim(smooth, 2), /normal

x1=0.6 & len=0.05 & textoff=0.01
y1=0.1 & ysep=0.03 & ytextoff=-0.005

plots, [x1, x1+len], [y1, y1], linestyle=0, /normal
xyouts, x1+len+textoff, y1-0*ysep+ytextoff, 'Outside, A/C inlet, chamber A', /normal
plots, [x1, x1+len], [y1-ysep, y1-ysep], linestyle=2, /normal
xyouts, x1+len+textoff, y1-1*ysep+ytextoff, 'Siderostat, A/C outlet, chamber B', /normal
plots, [x1, x1+len], [y1-2*ysep, y1-2*ysep], linestyle=1, /normal
xyouts, x1+len+textoff, y1-2*ysep+ytextoff, 'Trailer', /normal
plots, [x1, x1+len], [y1-3*ysep, y1-3*ysep], linestyle=3, /normal
xyouts, x1+len+textoff, y1-3*ysep+ytextoff, 'Spectrometer', /normal

multiplot, /reset

!p.multi = 0
!x.margin = xmarsave & !y.margin = ymarsave
!p.multi = 0
!p.psym=0
!x.style=0 & !x.ticks=0 & !x.minor=0

END
