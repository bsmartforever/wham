PRO plotstack,shift

    if not keyword_set(z) then shift = 0.

readcol,'~/Dropbox/lines_plot.dat',el1,xxx,w1,format='A,A,f'

w2 = STRTRIM(string(w1,'(f8.1)'),2)
files = el1+w2+'i.save'

count = n_elements(el1)
	if (count/2. eq fix(float(count/2.))) then begin 
	num = fix(float(count/2.))
 	endif else begin 
	num = fix(float(count/2. + 0.5))
	endelse

ys1 = count *0.49


 SET_PLOT, 'PS'
;
DEVICE, FILENAME='plotstack.eps', FONT_SIZE=11, /INCHES, XSIZE=7.5, YSIZE=ys1, $
	/ENCAPSULATED, /COLOR, /portrait

!P.MULTI = [0, 2, num, 0, 0]
!P.CHARSIZE = 2.
!p.psym=10
loadct,40
!x.ticklen = 0.04

	if (count eq 1) then step = (0.99 - 0.15)/float(num)
	if (count gt 1) then step = (0.99 - 0.81/float(count))/float(num)
	y2 = 0.99
	y1 = y2 - step
	x1 = 0.11
	x2 =0.53
	
	FOR i=0, count -1    DO BEGIN
	
	restore,files(i)
	fv1 = float(alog10(fval * wavc)) 
	index = where(v ge 200 and v le 400)

	jk = mean(f[index])
	flux = f/jk
	fm = max(flux) +0.1*max(flux)
	!y.tickinterval = fix(fm/2.)
	if fm le 3 then !y.tickinterval = 0.5
	vel = v +shift    
	
	
	if (i lt num-1) then begin  
	plot, vel, flux, xrange=[-399, 399], yrange = [-0.04,fm],nsum = 1, /xs, /ys,$
	xtickname=replicate(' ',10), charthick = 2,thick=2,$
	xtitle = ' ', ytitle = ' ', title = '', $
	position = [x1, y1, x2, y2]
	ver,0,lines=1,color =50, thick = 3
	ver,100,lines=1,color =150, thick = 3	
	ver,-100,lines=1,color =150, thick = 3	
	ver,vstar,lines=2,color =90, thick = 3	
	xyouts,x1+0.01,y1+0.01,ion+' !7k!6'+STRTRIM(string((wavc)),1), al=0, charsize = 1.2, charthick = 2,/normal
	if i eq 0 then xyouts,x1+0.01,y2-0.02,name, al=0, charsize = 1.2, charthick = 3, color = 230,/normal

	y2 = y2 - step
	y1 = y2 - step
	endif 
	
 	if (i eq num-1) then begin  
	plot, vel, flux, xrange=[-399, 399], yrange = [-0.04,fm],nsum = 1, /xs, /ys,$
	xtitle = '!6V (km s!e-1!n) ',  charthick = 2,thick=2,ytitle = ' ', title = '', $
	position = [x1, y1, x2, y2]
	ver,0,lines=1,color =50, thick = 3
	ver,100,lines=1,color =150, thick = 3	
	ver,-100,lines=1,color =150, thick = 3	
	ver,vstar,lines=2,color =90, thick = 3	
	xyouts,x1+0.01,y1+0.01,ion+' !7k!6'+STRTRIM(string((wavc)),1), al=0, charsize = 1.2, charthick = 2,/normal
	endif
	if (i eq num ) then begin 
		x1 = 0.57 
		x2 = 0.99
		y2 = 0.99
		y1 = y2 - step
	endif
	
	if (i ge num  and i lt count - 1) then begin
	plot, vel, flux, xrange=[-399, 399], yrange = [-0.04,fm],nsum = 1, /xs, /ys,$
	xtickname=replicate(' ',10),$
	xtitle = ' ', ytitle = ' ', title = '', charthick = 2,thick=2, $
	position = [x1, y1, x2, y2]
	ver,0,lines=1,color =50, thick = 3	
	ver,100,lines=1,color =150, thick = 3	
	ver,-100,lines=1,color =150, thick = 3	
	ver,vstar,lines=2,color =90, thick = 3	
	xyouts,x1+0.01,y1+0.01,ion+' !7k!6'+STRTRIM(string((wavc)),1), al=0, charsize = 1.2, charthick = 2,/normal

	y2 = y2 - step
	y1 = y2 - step
	endif 
	
	if (i eq count-1) then begin  
	plot, vel, flux, xrange=[-399, 399], yrange = [-0.04,fm],nsum = 1, /xs, /ys,$
	xtitle = '!6V (km s!e-1!n) ', ytitle = ' ', title = '', $
	charthick = 2, thick=2,$
	position = [x1, y1, x2, y2]	
	xyouts,x1+0.01,y1+0.01,ion+' !7k!6'+STRTRIM(string((wavc)),1), al=0, charsize = 1.2, charthick = 2,/normal
	ver,0,lines=1,color =50, thick = 3
	ver,100,lines=1,color =150, thick = 3	
	ver,-100,lines=1,color =150, thick = 3	
	ver,vstar,lines=2,color =90, thick = 3	

	endif

	ENDFOR 

xyouts,0.04,.53,'!6Relative Flux', orientation = 90, al = .5, charsize = 1.9, charthick = 2,/normal

DEVICE, /CLOSE

set_plot,'x'
cleanplot, /silent




END 
