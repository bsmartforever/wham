PRO plot_dark_time, log, begintime, endtime

observatory, 'ctio', obs

thick = !D.name EQ 'PS' ? 6 : 3
; morning dark time
darktime = get_dark_times(log[0].sysclock)
darktime -= obs.tz/24
IF darktime[1] GE begintime AND darktime[0] LE endtime THEN BEGIN
	plots, [darktime[0] > begintime, darktime[1] < endtime], $
		[!y.crange[0], !y.crange[0]], thick=thick
	plots, [darktime[0] > begintime, darktime[1] < endtime], $
		[!y.crange[1], !y.crange[1]], thick=thick
ENDIF

; evening dark time
darktime = get_dark_times(log[n_elements(log) - 1].sysclock)
darktime -= obs.tz/24
IF darktime[1] GE begintime AND darktime[0] LE endtime THEN BEGIN
	plots, [darktime[0] > begintime, darktime[1] < endtime], $
		[!y.crange[0], !y.crange[0]], thick=thick
	plots, [darktime[0] > begintime, darktime[1] < endtime], $
		[!y.crange[1], !y.crange[1]], thick=thick
ENDIF

END

PRO plot_notes, log, smooth, title=title

title = keyword_set(title) ? title : string(min(log.sysclock), format='(C())') + ' to ' + string(max(log.sysclock), format='(C())')

psymsave = !p.psym
!p.psym=0
siderocolor=64
siderostyle=0
trailercolor=127
trailerstyle=1
spectcolor=192
spectstyle=3
ct=6

x1=0.6 & len=0.05 & textoff=0.01
y1=0.19 & ysep=0.03 & ytextoff=-0.005

plots,[x1, x1+len],[y1, y1], linestyle=0, /normal
loadct, /silent, ct
plots,[x1, x1+len],[y1-ysep, y1-ysep], linestyle=siderostyle, /normal, color=siderocolor
plots,[x1, x1+len],[y1-2*ysep, y1-2*ysep], linestyle=trailerstyle, /normal, color=trailercolor
plots,[x1, x1+len],[y1-3*ysep, y1-3*ysep], linestyle=spectstyle, /normal, color=spectcolor
thick = !D.name EQ 'PS' ? 6 : 3
loadct, /silent, 0
plots,[x1, x1+len],[y1-4*ysep, y1-4*ysep], linestyle=0,/normal,thick=thick
xyouts, x1+len+textoff, y1-0*ysep+ytextoff, 'Outside, A/C inlet, chamber A', /normal
xyouts, x1+len+textoff, y1-1*ysep+ytextoff, 'Siderostat, A/C outlet, chamber B', /normal
xyouts, x1+len+textoff, y1-2*ysep+ytextoff, 'Trailer', /normal
xyouts, x1+len+textoff, y1-3*ysep+ytextoff, 'Spectrometer', /normal
xyouts, x1+len+textoff, y1-4*ysep+ytextoff, 'Dark time', /normal

firsthour = where(log.sysclock LE log[0].sysclock + 0.5/24.)
lasthour = where(log.sysclock GE log[n_elements(log)-1].sysclock - 0.5/24)

x1=0.03

loglen = strtrim(n_elements(log), 2) + ' minutes'

xyouts,x1,y1-0*ysep,/normal,'Cal mirror in beam: ' + $
	strtrim(fix(total(log.cm_beam)),2) + ' minutes'

xyouts, x1, y1-1*ysep, /normal, 'Lens carriage in beam: ' + $
	strtrim(fix(total(log.lc_beam)),2) + ' minutes '

xyouts,x1, y1-2*ysep, /normal,'Shutter open: ' + $
	strtrim(fix(total(log.shutter)),2) + ' minutes'

xyouts,x1, y1-3*ysep, /normal,'Cal lamp on:' + $
	' A: ' + strtrim(fix(total(log.callamp_a)), 2) + $
	'; B: ' + strtrim(fix(total(log.callamp_b)), 2) + $
	'; C: ' + strtrim(fix(total(log.callamp_c)), 2) + $
	'; D: ' + strtrim(fix(total(log.callamp_d)), 2) + ' minutes'
	
sf6weight = mean(log.sf6weight) - 119.
sf6percentage = sf6weight / 115. * 100
xyouts, x1, y1-4*ysep, /normal, 'SF!L6!N weight: ' + $
	string(sf6weight, format='(I3)') + ' lb. (' + $
	string(sf6percentage, format='(I3)') + '% remaining). With bottle: ' +$
	string(mean(log.sf6weight), format='(I3)') + ' lb.'

xyouts, x1, y1-5*ysep, /normal,'LN!L2!N fill enable ' + $
	strtrim(fix(total(log.ln2_fill)),2) + ' minutes'

xyouts, x1, y1-6*ysep, /normal, 'Total time: ' + $
	strtrim(fix(n_elements(log)), 2) + ' minutes'

xyouts, 0.9, 0.0, 'smooth=' + strtrim(smooth, 2), /normal

!p.psym=psymsave

END

;;;;;;; ploticsday ;;;;;;;

PRO ploticsday, logfile, pcslogfile, ccdpreslogfile,title=title,smooth=smooth, $
	begintime=begintime, endtime=endtime, onepage=onepage, keepunzip=keepunzip

log = trim_icslog(read_icslog(logfile, keepunzip=keyword_set(keepunzip)), /ignorecheck)
IF keyword_set(begintime) THEN log = log[where(log.sysclock GE begintime)]
IF keyword_set(endtime) THEN log = log[where(log.sysclock LE endtime)]

pcslog = trim_icslog(read_pcslog(pcslogfile, keepunzip=keyword_set(keepunzip)), /ignorecheck)
IF keyword_set(begintime) THEN BEGIN
	idx = where(pcslog.sysclock GE begintime)
	IF n_elements(idx) GT 1 THEN pcslog=pcslog[idx]
ENDIF
IF keyword_set(endtime) THEN BEGIN
	idx = where(pcslog.sysclock LE endtime)
	IF n_elements(idx) GT 1 THEN pcslog=pcslog[idx]
ENDIF

ccdpreslog = read_ccd_pressure_log(ccdpreslogfile, /keepunzip)
IF keyword_set(begintime) THEN BEGIN
	idx = where(ccdpreslog.sysclock GE begintime)
	IF array_equal(idx, -1) THEN $
		print, 'WARNING: No elements found with ccdpreslog.sysclock GE begintime' ELSE ccdpreslog=ccdpreslog[idx]
ENDIF
IF keyword_set(endtime) THEN BEGIN
	idx = where(ccdpreslog.sysclock LE endtime)
	IF array_equal(idx, -1) THEN $
		print,'WARNING: No elements found with ccdpreslog.sysclock LE endtime' $
	ELSE ccdpreslog=ccdpreslog[idx]
ENDIF
plotpres = n_elements(ccdpreslog) LT 2 ? 0 : 1

siderocolor=64
siderostyle=2
trailercolor=127
trailerstyle=1
spectcolor=192
spectstyle=3
ct=6

postscript = !D.name EQ 'PS' ? 1 : 0
xmarsave = !x.omargin & ymarsave = !y.omargin

IF keyword_set(onepage) THEN $
	!x.omargin = postscript ? [6, 65] : [10, 100] ELSE $
	!x.omargin = postscript ? [7, 8] : [10, 10]

IF (endtime - begintime LE 1.0) THEN BEGIN ; if this is a one day or less plot
	!x.ticks = 4 & !x.minor = 6
	xtickformat = '(C(CHI2.2, ":", CMI2.2))'
ENDIF ELSE BEGIN
	!x.ticks=2 & !x.minor=12
	xtickformat='(C(CMOI, "/", CDI, "/", CYI, " ", CHI2.2, ":", CMI2.2))'
ENDELSE
!x.ticklen = 0.05

notespageymar = [18., 2.0] & nonotesymar = [5.0, 2.0]
!y.omargin = keyword_set(onepage) ? notespageymar : nonotesymar
!x.style=1
psymsave=!p.psym
plotsym, 0, 0.2, /fill
!p.psym=0
smooth=keyword_set(smooth) ? smooth : 1

multiplot, [1, 6], /rowmajor ;;; Full temperatures

title = string(keyword_set(title) ? title : string(min(log.sysclock), $
	format='(C())') + ' to ' + string(max(log.sysclock), format='(C())')) + $
	' UTC' + string(log[0].tz, format='(I03)')

plot, smooth(log.sysclock, smooth), smooth(log.outside_temp, smooth), ytitle='Temp (C)', ys=8, yr=[-10, 40], title=title
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='Temp (F)'
loadct, /silent, ct
oplot, smooth(log.sysclock, smooth), smooth(log.siderostat_temp, smooth), linestyle=siderostyle, color=siderocolor
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_temp, smooth), linestyle=trailerstyle, color=trailercolor
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_temp, smooth), linestyle=spectstyle, color=spectcolor
loadct, /silent, 0

plot_dark_time, log, begintime, endtime

multiplot ;;; full relative humidity

plot, smooth(log.sysclock, smooth), smooth(log.outside_humid, smooth), $
	ytitle='% Rel humidity', yr=[0,110],/ys
loadct, /silent, ct
oplot, smooth(log.sysclock, smooth), smooth(log.siderostat_humid, smooth), linestyle=siderostyle, color=siderocolor
oplot, smooth(log.sysclock, smooth), smooth(log.trailer_humid, smooth), linestyle=trailerstyle, color=trailercolor
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_humid, smooth), linestyle=spectstyle, color=spectcolor
loadct, /silent, 0

plot_dark_time, log, begintime, endtime

multiplot ;;; Wind speed

plot,log.sysclock,log.windspeed, ytitle='Wind (mph)', yr=[0, 80], /ys

plot_dark_time, log, begintime, endtime

multiplot ;;; Wind direction

idx = where(log.windspeed GT 0.2)
plot, log[idx].sysclock, log[idx].winddir, psym=8, ytitle='Wind dir', $
	yticks=4, yr=[0, 360], /ys
axis,yaxis=1,ytickn=['N','E','S','W','N'],ytickv=[0,90,180,270,360],yticks=4

plot_dark_time, log, begintime, endtime

multiplot ;;; Baro pressure and heater

plot, smooth(log.sysclock, smooth), smooth(log.baropres, smooth), $
	ytitle='Baro pres (mm Hg)', yr=[580, 600], ys=25

offset = -5.0 & max=15.0
normheater=(log.heater-offset )/(max-offset)
heater_presunits =  normheater * (!y.crange[1]-!y.crange[0]) + !y.crange[0]
loadct, /silent, ct
oplot, log.sysclock, heater_presunits, color=siderocolor
axis, yaxis=1, ys=1, ytitle='Sidero heater (amp)', color=siderocolor, $
	yr=[offset, offset + (max - offset)], yticks=1, ytickv=[0, 10], yminor=5
loadct, /silent, 0

plot_dark_time, log, begintime, endtime

multiplot ;;; AC Temps

plot, smooth(log.sysclock, smooth), smooth(log.acin_temp, smooth), $
	ytitle='AC temp (C)', ys=24, yr=[0, 25], xtickformat=xtickformat
axis, yaxis=1, yr=(9./5)*!y.crange + 32., /ys, ytitle='AC Temp (F)'
loadct, /silent, ct
oplot, smooth(log.sysclock, smooth), smooth(log.acout_temp, smooth), $
	linestyle=siderostyle, color=siderocolor
loadct, /silent, 0

plot_dark_time, log, begintime, endtime

multiplot, /reset
IF keyword_set(onepage) THEN BEGIN
	!x.omargin = [!x.omargin[1], !x.omargin[0]] 
	title = ''
ENDIF ELSE BEGIN
	!y.omargin = notespageymar
	erase	; advance page
ENDELSE

multiplot, [1, 4], /rowmajor ;;; CCD Temp

plot, log.sysclock, log.ccdtemp, yr=[-120, 40], ytitle='CCD Temp (C)', /ys, $
	title=title

plot_dark_time, log, begintime, endtime

multiplot ;;; CCD vacuum

plot, ccdpreslog.sysclock, ccdpreslog.pressure*1000., $
	ytitle='CCD pres (mTorr)', yr=[0.7e0, 1e4], /ylog, /ys
;idx = where(ccdpreslog.ig_pres NE 0)
;IF NOT array_equal(idx, -1) THEN BEGIN
;	loadct, ct, /silent
;	oplot, ccdpreslog[idx].sysclock, ccdpreslog[idx].ig_pres*1000., $
;		color=siderocolor
;	loadct, 0, /silent
;ENDIF

multiplot ;;; Champer pressures

plot, smooth(pcslog.sysclock, smooth), smooth(pcslog.cha_pres, smooth), ys=16, $
	ytitle='Ch pres (mm Hg)', yr=[0, 3000]
loadct, ct, /silent
oplot, smooth(pcslog.sysclock, smooth), smooth(pcslog.chb_pres, smooth), $
	linestyle=siderostyle, color=siderocolor
loadct, 0, /silent

plot_dark_time, log, begintime, endtime

multiplot ;;; Champer temperatures

plot, smooth(pcslog.sysclock, smooth), smooth(pcslog.cha_temp, smooth), $
	ytitle='Ch temp (C)', ys=16, yr=[15, 25], xtickformat=xtickformat
loadct, ct, /silent
oplot, smooth(log.sysclock, smooth), smooth(log.spectrometer_temp, smooth), linestyle=spectstyle, color=spectcolor
loadct, 0, /silent
oplot, smooth(pcslog.sysclock, smooth), smooth(pcslog.cha_temp, smooth)
loadct, ct, /silent
oplot, smooth(pcslog.sysclock, smooth), smooth(pcslog.chb_temp, smooth), $
	linestyle=siderostyle, color=siderocolor
loadct, 0, /silent

plot_dark_time, log, begintime, endtime

plot_notes, log, smooth, title=keyword_set(title) ? title : 0

multiplot, /reset

!p.multi = 0
!x.omargin = xmarsave & !y.omargin = ymarsave
!p.multi = 0
!p.psym=psymsave
!x.style=0 & !x.ticks=0 & !x.minor=0

END
